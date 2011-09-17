package scala.tools.nsc.backend.jribble

import scala.collection.JavaConversions._
import scala.collection.{mutable=>mut}
import scala.tools.nsc._
import scala.tools.nsc.backend._
import scala.tools.nsc.backend.icode._
import scala.tools.nsc.backend.jribble.{JribbleProtos=>P}
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.util.{Set=>_, _}

/** Utilities to convert scalac ASTs to Jribble protobufs */
abstract class ProtobufConverter extends AnyRef with JribbleAnalysis {
  val global: Global
  import global._
  val typeKinds: TypeKinds {
    val global: ProtobufConverter.this.global.type
  }
  import typeKinds._
  val scalaPrimitives: ScalaPrimitives {
    val global: ProtobufConverter.this.global.type
  }
  import scalaPrimitives._
  val freshNames: FreshNameCreator
      
  private def trace[T](tree: Tree)(process: =>T): T =
    try {
      process
    } catch {
      case ex:Exception =>
        Console.println("Exception while traversing: " + tree)
        Console.out.flush
        throw ex
    }
    
  //copied from GenJVM
  def moduleSuffix(sym: Symbol) = {
    import scala.reflect.generic.Flags
    if (sym.hasFlag(Flags.MODULE) && !sym.isMethod &&
       !sym.isImplClass && !sym.hasFlag(Flags.JAVA)) "$"
    else "";
  }

  def globalName(sym: Symbol, addModuleSuffix: Boolean): P.GlobalName = {
    val proto = P.GlobalName.newBuilder
    
    if (!sym.owner.isEffectiveRoot)
      proto.setPkg(sym.owner.enclClass.fullName)
    
    val suffix = if (addModuleSuffix) moduleSuffix(sym) else ""
    proto.setName(sym.name.encode.toString + suffix)
    
    proto.build
  }

  def globalName(sym: Symbol): P.GlobalName =
    globalName(sym, true)

  def moduleMirrorGlobalName(sym: Symbol): P.GlobalName =
    globalName(sym, false)
    
  def modifiers(sym: Symbol): P.Modifiers = {
    val proto = P.Modifiers.newBuilder
    val flags = sym.flags

    def ifFlag(f: Long)(action: =>Unit) {
      if ((f & flags) != 0)
        action
    }

    if ((flags & (PRIVATE | PROTECTED)) == 0 && sym.owner.isClass)
      proto.setIsPublic(true)
    else {
      ifFlag(PRIVATE) { proto.setIsPrivate(true) }
      //TODO(grek): Check comment in GenJVM's javaFlags method which says that
      //protected modifier shouldn't be emitted
      ifFlag(PROTECTED) { proto.setIsProtected(true) }
    }

    if (sym.isStaticMember)
      proto.setIsStatic(true)

    if (!sym.enclClass.hasFlag(INTERFACE) && !sym.isClassConstructor)
      ifFlag(FINAL) { proto.setIsFinal(true) }

    if ((flags & (DEFERRED | ABSTRACT)) != 0 && !sym.enclClass.hasFlag(INTERFACE))
      proto.setIsAbstract(true)

    proto.build
  }
    
  /** Convert a Scala name to a Jribble name */
  def symName(symbol: Symbol, name: Name): String = {
    if (symbol == definitions.NothingClass)
      "Lscala/runtime/Nothing$;"
    else if (symbol == definitions.NullClass)
      "Lscala/runtime/Null$;"
    else if (symbol != null && symbol != NoSymbol)
      symbol.simpleName.encode.toString
    else name.encode.toString
  }
   
  def convert(tpe: Type): P.Type = {
    def cvt(typ: TypeKind): P.Type = {
      val proto = P.Type.newBuilder
      typ match {
        case UNIT =>
          proto.setType(P.Type.TypeType.Void)
        case BOOL =>
          proto.setType(P.Type.TypeType.Primitive);
          proto.setPrimitiveType(P.PrimitiveType.Boolean)
        case BYTE =>
          proto.setType(P.Type.TypeType.Primitive);
          proto.setPrimitiveType(P.PrimitiveType.Byte)
        case SHORT =>
          proto.setType(P.Type.TypeType.Primitive);
          proto.setPrimitiveType(P.PrimitiveType.Short)
        case CHAR =>
          proto.setType(P.Type.TypeType.Primitive);
          proto.setPrimitiveType(P.PrimitiveType.Char)
        case INT =>
          proto.setType(P.Type.TypeType.Primitive);
          proto.setPrimitiveType(P.PrimitiveType.Int)
        case LONG =>
          proto.setType(P.Type.TypeType.Primitive);
          proto.setPrimitiveType(P.PrimitiveType.Long)
        case FLOAT =>
          proto.setType(P.Type.TypeType.Primitive);
          proto.setPrimitiveType(P.PrimitiveType.Float)
        case DOUBLE =>
          proto.setType(P.Type.TypeType.Primitive);
          proto.setPrimitiveType(P.PrimitiveType.Double)
        case REFERENCE(cls)  =>
          proto.setType(P.Type.TypeType.Named)
          cls match {
            case definitions.NothingClass =>
              val name = P.GlobalName.newBuilder
              name.setPkg("scala.runtime")
              name.setName("Nothing$")
              proto.setNamedType(name)
            case definitions.NullClass =>
              val name = P.GlobalName.newBuilder
              name.setPkg("scala.runtime")
              name.setName("Null$")
              proto.setNamedType(name)
            case _ =>
              proto.setNamedType(globalName(cls))
          }
        case ARRAY(elem)     => 
          proto.setType(P.Type.TypeType.Array)
          proto.setArrayElementType(cvt(elem))
      }
      proto.build
    }
    return cvt(toTypeKind(tpe))
  }
  
  def convertParam(param: ValDef): P.ParamDef = trace(param) { param match { 
    case ValDef(mods, name, tp, rhs) =>
      val builder = P.ParamDef.newBuilder
      builder.setName(symName(param.symbol, name))
      builder.setTpe(convert(tp.tpe))
      builder.build
  } }
  
  
  def methodSignature(s: Symbol): P.MethodSignature = {
    val proto = P.MethodSignature.newBuilder
    //symbol can be both constructor and static member
    //this happens if you have a constructor defined
    //for a trait. Such constructor is represented
    //as a static method called $init$, we need to
    //take that into account when emitting signatures
    if (s.isConstructor && !s.isStaticMember)
      proto.setName("new")
    else
      proto.setName(s.name.encode.toString)
    proto.setOwner(globalName(s.owner))
    for (tp <- s.tpe.paramTypes)
      proto.addParamType(convert(tp))
    proto.setReturnType(convert(s.tpe.resultType))
    proto.build
  }
  
  def loadModule(module: Symbol, exprBuilder: P.Expr.Builder) {
    exprBuilder.setType(P.Expr.ExprType.FieldRef)
    val fieldRef = P.FieldRef.newBuilder
    fieldRef.setEnclosingType(globalName(module))
    fieldRef.setName(nme.MODULE_INSTANCE_FIELD.toString)
    exprBuilder.setFieldRef(fieldRef.build)
  }
  
  def unaryOp(prim: Int): Option[P.Unary.UnaryOp] =
    prim match {
	  case NEG => Some(P.Unary.UnaryOp.Neg)
	  case NOT => Some(P.Unary.UnaryOp.BitNot)
	  case POS => Some(P.Unary.UnaryOp.Pos)
	  case ZNOT => Some(P.Unary.UnaryOp.Not)
	  case _ => None
    }
  
  def binaryOp(prim: Int): Option[P.Binary.BinaryOp] = 
    prim match {
	  // Arithmetic binary operations
	  case ADD => Some(P.Binary.BinaryOp.Plus)
	  case SUB => Some(P.Binary.BinaryOp.Minus)
	  case MUL => Some(P.Binary.BinaryOp.Multiply)
	  case DIV => Some(P.Binary.BinaryOp.Divide)
	  case MOD => Some(P.Binary.BinaryOp.Modulus)
	
	  // Bitwise operations
	  case OR  => Some(P.Binary.BinaryOp.BitOr)
	  case XOR => Some(P.Binary.BinaryOp.BitXor)
	  case AND => Some(P.Binary.BinaryOp.BitAnd)
	
	  // Shift operations
	  case LSL => Some(P.Binary.BinaryOp.BitLeftShift)
	  case LSR => Some(P.Binary.BinaryOp.BitUnsignedRightShift)
	  case ASR => Some(P.Binary.BinaryOp.BitRightShift)
	
	  // Comparison operations
	  case ID => Some(P.Binary.BinaryOp.Equal)
	  case NI => Some(P.Binary.BinaryOp.NotEqual)
	  case EQ => Some(P.Binary.BinaryOp.Equal)
	  case NE => Some(P.Binary.BinaryOp.NotEqual)
	  case LT => Some(P.Binary.BinaryOp.Lesser)
	  case LE => Some(P.Binary.BinaryOp.LesserOrEqual)
	  case GT => Some(P.Binary.BinaryOp.Greater)
	  case GE => Some(P.Binary.BinaryOp.GreaterOrEqual)
	
	  // Boolean binary operations
	  case ZOR => Some(P.Binary.BinaryOp.Or)
	  case ZAND => Some(P.Binary.BinaryOp.And)
	
	  // String operations
	  case CONCAT => Some(P.Binary.BinaryOp.Concat)
	    
	  case _ => None
    }
  
  def convertLiteral(constant: Constant): P.Literal = {
    val proto = P.Literal.newBuilder
    constant.value match {
      case x:Boolean => proto.setType(P.Literal.LiteralType.Boolean); proto.setBoolValue(x)
      case x:Byte => proto.setType(P.Literal.LiteralType.Byte); proto.setByteValue(x)
      case x:Char => proto.setType(P.Literal.LiteralType.Char); proto.setCharValue(x)
      case x:Double => proto.setType(P.Literal.LiteralType.Double); proto.setDoubleValue(x)
      case x:Float => proto.setType(P.Literal.LiteralType.Float); proto.setFloatValue(x)
      case x:Int => proto.setType(P.Literal.LiteralType.Int); proto.setIntValue(x)
      case x:Long => proto.setType(P.Literal.LiteralType.Long); proto.setLongValue(x)
      case null => proto.setType(P.Literal.LiteralType.Null)
      case x:Short => proto.setType(P.Literal.LiteralType.Short); proto.setShortValue(x)
      case x:String => proto.setType(P.Literal.LiteralType.String); proto.setStringValue(x)

      // TODO(grek) Delete this case. When building the Jribble library,
      // you will see that an invalid expression "$1$ = ()" is making
      // it to this point of the code, even though the $1$ variable in
      // question is of type boolean.
      case x : scala.runtime.BoxedUnit => proto.setType(P.Literal.LiteralType.Boolean); proto.setBoolValue(false)
    }
    proto.build
  }
  
  def convertExpr(tree: Tree, enclosingClass: Symbol): P.Expr = trace(tree) {
    def cvt(t: Tree) = convertExpr(t, enclosingClass)

    val proto = P.Expr.newBuilder
    (tree: @unchecked) match {
      case Apply(t @ Select(New(tpt), nme.CONSTRUCTOR), args)
      if (tpt.tpe.typeSymbol == definitions.ArrayClass) =>
        def extractDims(tpe: Type): (Int, Type) = if (tpe.typeSymbol == definitions.ArrayClass) {
          (tpe: @unchecked) match {
            case TypeRef(_, _, List(elemType)) => {
              val (dims, tpe) = extractDims(elemType)
              (dims+1, tpe)
            }
          }
        } else (0, tpe)
        val (dims, elemType) = extractDims(tpt.tpe)
        assert(args.size <= dims)
        
        proto.setType(P.Expr.ExprType.NewArray)
        val newArray = P.NewArray.newBuilder
        newArray.setElementType(convert(elemType))
        newArray.setDimensions(dims)
        for (arg <- args) newArray.addDimensionExpr(cvt(arg))
        proto.setNewArray(newArray.build)

      case ArrayValue(elemtpt, trees) =>
        proto.setType(P.Expr.ExprType.NewArray)
        val newArray = P.NewArray.newBuilder
        newArray.setElementType(convert(elemtpt.tpe))
        newArray.setDimensions(1)
        for (init <- trees) newArray.addInitExpr(cvt(init))
        proto.setNewArray(newArray.build)

      case Apply(fun @ Select(receiver, name), args)
      if isPrimitive(fun.symbol) =>
        val prim = getPrimitive(fun.symbol) 
        if (unaryOp(prim).isDefined) {
          proto.setType(P.Expr.ExprType.Unary)
          val unary = P.Unary.newBuilder
          unary.setOp(unaryOp(prim).get)
          unary.setExpr(cvt(receiver))
          proto.setUnary(unary.build)
        } else if (binaryOp(prim).isDefined) {
          proto.setType(P.Expr.ExprType.Binary)
          val binary = P.Binary.newBuilder
          binary.setOp(binaryOp(prim).get)
          binary.setLhs(cvt(receiver))
          binary.setRhs(cvt(args.head))
          binary.setTpe(convert(tree.tpe))
          proto.setBinary(binary.build)
        } else if (prim == APPLY) {
          proto.setType(P.Expr.ExprType.ArrayRef)
          val arrayRef = P.ArrayRef.newBuilder
          arrayRef.setArray(cvt(receiver))
          arrayRef.setIndex(cvt(args.head))
          proto.setArrayRef(arrayRef)
        } else if (prim == UPDATE) {
          // array update
          // rcvr[args(0)] = args(1)
          proto.setType(P.Expr.ExprType.Assignment)
          val assignment = P.Assignment.newBuilder;
          {
            val lhs = P.Expr.newBuilder
            lhs.setType(P.Expr.ExprType.ArrayRef)
            val arrayRef = P.ArrayRef.newBuilder
            arrayRef.setArray(cvt(receiver))
            arrayRef.setIndex(cvt(args(0)))
            lhs.setArrayRef(arrayRef.build)
            assignment.setLhs(lhs.build)
          }
          assignment.setRhs(cvt(args(1)))
          proto.setAssignment(assignment)
        } else if (prim == LENGTH) {
          proto.setType(P.Expr.ExprType.ArrayLength)
          val arrayLength = P.ArrayLength.newBuilder
          arrayLength.setArray(cvt(receiver))
          proto.setArrayLength(arrayLength)
        } else {
          throw new IllegalArgumentException("Unhandled primitive (" + prim + ") for " + tree)
        }
        
      case Apply(TypeApply(fun@Select(rcvr, _), List(tpe)), Nil)
      if fun.symbol == definitions.Object_asInstanceOf =>
        proto.setType(P.Expr.ExprType.Cast)
        val cast = P.Cast.newBuilder
        cast.setExpr(cvt(rcvr))
        cast.setTpe(convert(tpe.tpe))
        proto.setCast(cast.build)
        
      case Apply(TypeApply(fun@Select(rcvr, _), List(tpe)), Nil)
      if fun.symbol == definitions.Object_isInstanceOf =>
        proto.setType(P.Expr.ExprType.InstanceOf)
        val instanceOf = P.InstanceOf.newBuilder
        instanceOf.setExpr(cvt(rcvr))
        instanceOf.setTpe(convert(tpe.tpe))
        proto.setInstanceOf(instanceOf.build)

      case Apply(fun @ Select(_: New, nme.CONSTRUCTOR), args)
      if tree.symbol.isConstructor =>
        proto.setType(P.Expr.ExprType.NewObject)
        val newObject = P.NewObject.newBuilder
        newObject.setClazz(globalName(fun.symbol.enclClass))
        newObject.setSignature(methodSignature(fun.symbol))
        for (arg <- args)
          newObject.addArgument(cvt(arg))
        proto.setNewObject(newObject.build)

      case tree@Apply(Select(receiver, _), args)
      if tree.symbol != NoSymbol =>
        proto.setType(P.Expr.ExprType.MethodCall)
        val methodCall = P.MethodCall.newBuilder
        methodCall.setSignature(methodSignature(tree.symbol))
        if (tree.symbol.isConstructor) {
          // Chaining to another constructor. Always use this.
          val thisRef = P.Expr.newBuilder
          thisRef.setType(P.Expr.ExprType.ThisRef)
          methodCall.setReceiver(thisRef)
        } else if (!tree.symbol.isStaticMember) {
          methodCall.setReceiver(cvt(receiver))
        }
        for (arg <- args)
          methodCall.addArgument(cvt(arg))
        proto.setMethodCall(methodCall.build)
        
                
      case tree@Select(qualifier, selector)
      if tree.symbol.isModule =>
        loadModule(tree.symbol, proto)

      case tree@This(_) if tree.symbol.isModuleClass && tree.symbol != enclosingClass =>
        loadModule(tree.symbol, proto)
        
      case This(_) =>
        proto.setType(P.Expr.ExprType.ThisRef)
        
      case Super(_, _) =>
        proto.setType(P.Expr.ExprType.SuperRef)

      case If(cond, exp1, exp2) =>
        proto.setType(P.Expr.ExprType.Conditional)
        val conditional = P.Conditional.newBuilder
        conditional.setCondition(cvt(cond))
        conditional.setTpe(convert(tree.tpe))
        conditional.setThen(cvt(exp1))
        conditional.setElsee(cvt(exp2))
        proto.setConditional(conditional.build)

      case Literal(c@Constant(x: Type))
      if c.tag == ClassTag =>
        proto.setType(P.Expr.ExprType.ClassLiteral)
        val classLiteral = P.ClassLiteral.newBuilder
        classLiteral.setTpe(convert(x))
        proto.setClassLiteral(classLiteral)

      case Literal(constant: Constant) =>
        proto.setType(P.Expr.ExprType.Literal)
        proto.setLiteral(convertLiteral(constant))

      case tree: Ident
      if !tree.symbol.isPackage && tree.symbol.isModule =>
        loadModule(tree.symbol, proto)
        
      case tree: Ident =>
        proto.setType(P.Expr.ExprType.VarRef)
        val varRef = P.VarRef.newBuilder
        varRef.setName(tree.symbol.name.encode.toString)
        proto.setVarRef(varRef.build)
        
      case tree@Select(qualifier, name) =>
        proto.setType(P.Expr.ExprType.FieldRef)
        val fieldRef = P.FieldRef.newBuilder
        if (!tree.symbol.isStaticMember) {
          fieldRef.setQualifier(cvt(qualifier))
        }
        fieldRef.setEnclosingType(globalName(tree.symbol.enclClass))
        fieldRef.setName(tree.symbol.name.encode.toString)
        proto.setFieldRef(fieldRef)
        
      case tree@Assign(lhs, rhs) =>
        proto.setType(P.Expr.ExprType.Assignment)
        val assignment = P.Assignment.newBuilder
        assignment.setLhs(convertExpr(lhs, enclosingClass))
        assignment.setRhs(convertExpr(rhs, enclosingClass))
        proto.setAssignment(assignment)

      case Typed(expr, _) =>
        return convertExpr(expr, enclosingClass)
    }
    
    proto.build
  }
  
  def convertCatch(
    catchClause: CaseDef,
    enclosingClass: Symbol,
    labelSyms: Set[Symbol]): P.Catch =
  {
    val proto = P.Catch.newBuilder
    
    catchClause match {
      case CaseDef(exBinding @ Bind(exName, _), _, _) =>
        val tpe: P.Type = convert(exBinding.symbol.tpe)
        assert(tpe.getType == P.Type.TypeType.Named)
        proto.setTpe(tpe.getNamedType)
        proto.setParam(exName.encode.toString)
        
      case CaseDef(Typed(Ident(nme.WILDCARD), tpt), _, catchBody) =>
        val tpe: P.Type = convert(tpt.tpe)
        assert(tpe.getType == P.Type.TypeType.Named)
        proto.setTpe(tpe.getNamedType)
        proto.setParam(freshNames.newName)

      case CaseDef(Ident(nme.WILDCARD), _, catchBody) =>
        proto.setTpe(globalName(definitions.ThrowableClass))
        proto.setParam(freshNames.newName)
    }
    
    proto.setBody(convertStatement(catchClause.body, enclosingClass, labelSyms))
    proto.build
  }

  def convertStatement(
    tree: Tree,
    enclosingClass: Symbol,
    labelSyms: Set[Symbol]): P.Statement =
  {
    def convertStat(stat: Tree) =
        convertStatement(stat, enclosingClass, labelSyms)

    val proto = P.Statement.newBuilder
    tree match {
      case Block(stats, expr) =>
        // The expression will be a nop, after Jribble normalization
        proto.setType(P.Statement.StatementType.Block)
        val block = P.Block.newBuilder
        for (stat <- stats) block.addStatement(convertStat(stat))
        proto.setBlock(block)

      case tree@LabelDef(_, _, innerStatement) =>
        val labelString = tree.symbol.name.encode.toString

        proto.setType(P.Statement.StatementType.LabelledStat)
        val labelledStat = P.LabelledStat.newBuilder
        labelledStat.setLabel(labelString)

        // Insert a while(true) so that the label can
        // be jumped to using continue
        val whileLoopStat = P.Statement.newBuilder
        whileLoopStat.setType(P.Statement.StatementType.While)
        val whileLoop = P.While.newBuilder
        val trueExp = P.Expr.newBuilder
        trueExp.setType(P.Expr.ExprType.Literal)
        trueExp.setLiteral(convertLiteral(Constant(true)))
        whileLoop.setCondition(trueExp)
        val whileBlockStat = P.Statement.newBuilder
        whileBlockStat.setType(P.Statement.StatementType.Block)
        val whileBlock = P.Block.newBuilder

        whileBlock.addStatement(
          convertStatement(
            innerStatement, 
            enclosingClass, 
            labelSyms + tree.symbol))

        val breakStat = P.Statement.newBuilder
        breakStat.setType(P.Statement.StatementType.Break)
        val break = P.Break.newBuilder
        break.setLabel(labelString)
        breakStat.setBreak(break)
        whileBlock.addStatement(breakStat)
        whileBlockStat.setBlock(whileBlock)
        whileLoop.setBody(whileBlockStat)
        whileLoopStat.setWhileStat(whileLoop)
        labelledStat.setStatement(whileLoopStat)

        proto.setLabelledStat(labelledStat)

      case tree:Apply if labelSyms.contains(tree.symbol) =>
        proto.setType(P.Statement.StatementType.Continue)
        val cont = P.Continue.newBuilder
        cont.setLabel(tree.symbol.name.encode.toString)
        proto.setContinueStat(cont)
      
      case If(cond, exp1, exp2) =>
        proto.setType(P.Statement.StatementType.If)
        val ifStat = P.If.newBuilder
        ifStat.setCondition(convertExpr(cond, enclosingClass))
        ifStat.setThen(convertStat(exp1))
        if (!exp2.isEmpty) {
          ifStat.setElsee(convertStat(exp2))
        }
        proto.setIfStat(ifStat)
      
      case Try(block, catches, finalizer) =>
        proto.setType(P.Statement.StatementType.Try)
        val tryStat = P.Try.newBuilder
        tryStat.setBlock(convertStat(block))
        for (catchClause <- catches) tryStat.addCatch(convertCatch(catchClause, enclosingClass, labelSyms))
        if (!finalizer.isEmpty) tryStat.setFinalizer(convertStat(finalizer))
        proto.setTryStat(tryStat)

      case tree@Match(selector, cases) =>
        assert(cases forall { case CaseDef(_, guard, _) => guard == EmptyTree })
        proto.setType(P.Statement.StatementType.Switch)
        val switch = P.Switch.newBuilder
        switch.setExpression(convertExpr(selector, enclosingClass))

        val switches: List[(List[Int], Tree)] = cases flatMap {
          case CaseDef(Literal(Constant(x: Int)), _, body) => (x :: Nil, body) :: Nil
          case tree@CaseDef(Alternative(xs), _, body) =>
            val constants = xs map { case Literal(Constant(x: Int)) => x }
            (constants, body) :: Nil
          case CaseDef(Ident(nme.WILDCARD), _, _) => Nil //this is handled separately
        }
        val default: Option[Tree] = {
          val trees = cases collect { case CaseDef(Ident(nme.WILDCARD), _, body) => body }
          assert(trees.size <= 1)
          trees.headOption
        }
        for ((constants,stat) <- switches; constant <- constants) {
          val caseClause = P.Case.newBuilder
          val lit = P.Literal.newBuilder
          lit.setType(P.Literal.LiteralType.Int)
          lit.setIntValue(constant)
          caseClause.setConstant(lit)
          caseClause.setStatement(convertStat(stat))
          switch.addCase(caseClause)
        }
        if (default.isDefined) switch.setDefaultCase(convertStat(default.get))
        
        proto.setSwitchStat(switch)
      
      case Throw(expr) =>
        proto.setType(P.Statement.StatementType.Throw)
        val throwStat = P.Throw.newBuilder
        throwStat.setExpression(convertExpr(expr, enclosingClass))
        proto.setThrowStat(throwStat)

      case Return(expr) =>
        proto.setType(P.Statement.StatementType.Return)
        val ret = P.Return.newBuilder
        if (!isUnit(expr)) ret.setExpression(convertExpr(expr, enclosingClass))
        proto.setReturnStat(ret)
        
      case tree:ValDef =>
        proto.setType(P.Statement.StatementType.VarDef)
        val varDef = P.VarDef.newBuilder
        varDef.setName(tree.symbol.simpleName.encode.toString)
        varDef.setTpe(convert(tree.symbol.tpe))
        if (!tree.rhs.isEmpty) {
          varDef.setInitializer(convertExpr(tree.rhs, enclosingClass))
        }
        proto.setVarDef(varDef)
        
      case exp =>
        // Assume it's an expression statement
        proto.setType(P.Statement.StatementType.Expr)
        proto.setExpr(convertExpr(exp, enclosingClass))
    }
    proto.build
  }
  
  def paramDef(param: ValDef): P.ParamDef = trace(param) {
    val proto = P.ParamDef.newBuilder
    proto.setName(param.symbol.simpleName.encode.toString)
    proto.setTpe(convert(param.symbol.tpe))
    proto.build
  }
  
  def declaration(tree: Tree, enclosingClass: Symbol): P.Declaration = trace(tree) {
    val proto = P.Declaration.newBuilder
    proto.setModifiers(modifiers(tree.symbol))

    tree match {
      case ValDef(_, name, _, rhs) =>
        proto.setType(P.Declaration.DeclarationType.Field)
        val fieldDef = P.FieldDef.newBuilder
        fieldDef.setName(tree.symbol.simpleName.encode.toString)
        fieldDef.setTpe(convert(tree.symbol.tpe))
        if (!rhs.isEmpty) fieldDef.setInitializer(convertExpr(rhs, enclosingClass))
        proto.setFieldDef(fieldDef)

      case tree@DefDef(mods, name, tparams, vparamss, tp, rhs) =>
        proto.setType(P.Declaration.DeclarationType.Method)
        val method = P.Method.newBuilder
        if (isConstructor(tree)) {
          method.setIsConstructor(true)
          method.setName("new")
        } else {
          method.setName(tree.symbol.simpleName.encode.toString)
        }
        for (paramList <- vparamss; param <- paramList) {
          method.addParamDef(paramDef(param))
        }
        method.setReturnType(convert(tp.tpe))
        if (!rhs.isEmpty) {
          val bodyProto = convertStatement(rhs, enclosingClass, Set.empty)
          // Add an initializer to MODULE$ if need be
          val clazz = tree.symbol.owner
          val updatedBodyProto =
            if (isStaticModule(clazz) && name == nme.CONSTRUCTOR) {
              addModuleInitializer(bodyProto, clazz)
            } else {
              bodyProto
            }

          method.setBody(updatedBodyProto)
        }
        proto.setMethod(method)
    }

    proto.build
  }
  
  def initializer(valDef: ValDef, enclosingClass: Symbol): P.Statement = {
    val statement = P.Statement.newBuilder
    statement.setType(P.Statement.StatementType.Expr)
    val expr = P.Expr.newBuilder
    expr.setType(P.Expr.ExprType.Assignment)
    val assignment = P.Assignment.newBuilder
    val lhs = P.Expr.newBuilder
    lhs.setType(P.Expr.ExprType.VarRef)
    val varRef = P.VarRef.newBuilder
    varRef.setName(valDef.symbol.name.encode.toString)
    lhs.setVarRef(varRef)
    assignment.setLhs(lhs)
    assignment.setRhs(convertExpr(valDef.rhs, enclosingClass))
    expr.setAssignment(assignment)
    statement.setExpr(expr)
    statement.build
  }
  
  /** Convert a class or an interface. This returns a builder so that
   *  GenJribble can add forwarding methods for singleton objects.
   */
  def declaredType(clazz: ClassDef): P.DeclaredType.Builder = trace(clazz) {
    val proto = P.DeclaredType.newBuilder
    proto.setName(globalName(clazz.symbol))
    if (isInterface(clazz.symbol)) proto.setIsInterface(true)
    proto.setModifiers(modifiers(clazz.symbol))

    val (parentInterfaces, parentClasses) = 
      clazz.impl.parents.map(_.symbol).partition(isInterface)
    assert(parentClasses.size <= 1) 
    
    if (!parentClasses.isEmpty) proto.setExt(globalName(parentClasses.head))
    for (parentInterface <- parentInterfaces) proto.addImplements(globalName(parentInterface))
    
    for (decl <- clazz.impl.body) proto.addMember(declaration(decl, clazz.symbol))

    if (isStaticModule(clazz.symbol)) {
      // TODO(spoon) this should be done in a tree rewrite, not in the middle of protobuf conversion
      val constructorSymbol = definitions.getMember(clazz.symbol, nme.CONSTRUCTOR)
      val decl = P.Declaration.newBuilder
      decl.setType(P.Declaration.DeclarationType.Field)
      val mods = P.Modifiers.newBuilder
      mods.setIsPublic(true)
      mods.setIsStatic(true)
      decl.setModifiers(mods)
      val fieldDef = P.FieldDef.newBuilder
      fieldDef.setName(nme.MODULE_INSTANCE_FIELD.encode.toString)
      val fieldType = P.Type.newBuilder
      fieldType.setType(P.Type.TypeType.Named)
      fieldType.setNamedType(globalName(clazz.symbol))
      fieldDef.setTpe(fieldType)
      val fieldInitializer = P.Expr.newBuilder
      fieldInitializer.setType(P.Expr.ExprType.NewObject)
      val newObj = P.NewObject.newBuilder
      newObj.setClazz(globalName(clazz.symbol))
      newObj.setSignature(methodSignature(constructorSymbol))
      fieldInitializer.setNewObject(newObj)
      fieldDef.setInitializer(fieldInitializer)
      decl.setFieldDef(fieldDef)
      proto.addMember(decl)
    }

    proto
  }
  
  /** Add a forwarder for a method in a singleton object */
  def addForwarder(module: Symbol, method: Symbol, mirror: P.DeclaredType.Builder) {
    val decl = P.Declaration.newBuilder
    decl.setType(P.Declaration.DeclarationType.Method)
    val mods = P.Modifiers.newBuilder
    mods.setIsFinal(true)
    mods.setIsStatic(true)
    mods.setIsPublic(true)
    decl.setModifiers(mods)
    
    val methodProto = P.Method.newBuilder
    methodProto.setName(method.name.encode.toString)
 
    val paramTypes = method.tpe.paramTypes
    for (i <- 0 until paramTypes.length) {
      val param = P.ParamDef.newBuilder
      param.setName("x_" + i)
      param.setTpe(convert(paramTypes(i)))
      methodProto.addParamDef(param)
    }
    
    methodProto.setReturnType(convert(method.tpe.resultType))

    val callExpr = P.Expr.newBuilder
    callExpr.setType(P.Expr.ExprType.MethodCall)
    val call = P.MethodCall.newBuilder
    val receiver = P.Expr.newBuilder
    loadModule(module, receiver)
    call.setReceiver(receiver)    
    call.setSignature(methodSignature(method))
    for (i <- 0 until paramTypes.length) {
      val arg = P.Expr.newBuilder
      arg.setType(P.Expr.ExprType.VarRef)
      val varRef = P.VarRef.newBuilder
      varRef.setName("x_" + i)
      arg.setVarRef(varRef)
      call.addArgument(arg)
    }
    callExpr.setMethodCall(call)
    
    val body = P.Statement.newBuilder
    if (isUnit(method.tpe.resultType)) {
      body.setType(P.Statement.StatementType.Expr)
      body.setExpr(callExpr)
    } else {
      body.setType(P.Statement.StatementType.Return)
      val ret = P.Return.newBuilder
      ret.setExpression(callExpr)
      body.setReturnStat(ret)
    }
    methodProto.setBody(body)

    decl.setMethod(methodProto)
    mirror.addMember(decl)
  }
  
  
  /**
   * Adds static forwarders for methods defined in modules (objects).
   *
   * Copied from GenJVM.
   * 
   * This would be better as an AST rewrite, but for the meantime it
   * is called directly by GenJribble.
   */
  def addForwarders(module: Symbol, mirror: P.DeclaredType.Builder) {
    def conflictsIn(cls: Symbol, name: Name) =
        cls.info.members exists (_.name == name)

    /** List of parents shared by both class and module, so we don't add forwarders
     *  for methods defined there - bug #1804 */
    lazy val commonParents = {
      val cps = module.info.baseClasses
      val mps = module.companionClass.info.baseClasses
      cps.filter(mps contains)
    }
    /* The setter doesn't show up in members so we inspect the name
    * ... and clearly it helps to know how the name is encoded, see ticket #3004.
    * This logic is grossly inadequate! Name mangling needs a devotee.
    */
    def conflictsInCommonParent(name: Name) =
      commonParents exists { cp =>
        (name startsWith (cp.name + "$")) || (name containsName ("$" + cp.name + "$"))
      }

    /** Should method `m' get a forwarder in the mirror class? */
    def shouldForward(m: Symbol): Boolean =
      atPhase(currentRun.picklerPhase) (
        m.owner != definitions.ObjectClass
          && m.isMethod
          && !m.hasFlag(CASE | PRIVATE | PROTECTED | DEFERRED | SPECIALIZED)
          && !m.isConstructor
          && !m.isStaticMember
          && !(m.owner == definitions.AnyClass)
          && !module.isSubClass(module.companionClass)
          && !conflictsIn(definitions.ObjectClass, m.name)
          && !conflictsInCommonParent(m.name)
          && !conflictsIn(module.companionClass, m.name)
      )

    assert(module.isModuleClass)
    if (settings.debug.value)
      log("Dumping mirror class for object: " + module);

    for (m <- module.info.nonPrivateMembers; if shouldForward(m)) {
      log("Adding static forwarder '%s' to '%s'".format(m, module))
      addForwarder(module, m, mirror)
    }
  }
  
  /** Create a mirror class for a given singleton object. This should really be in
   *  a tree rewrite, but it is here for now. */
  def mirrorClassFor(module: Symbol): P.DeclaredType = {
    val proto = P.DeclaredType.newBuilder
    proto.setName(moduleMirrorGlobalName(module))
    val mods = P.Modifiers.newBuilder
    mods.setIsFinal(true)
    mods.setIsPublic(true)
    proto.setModifiers(mods)

    addForwarders(module, proto)
    proto.build
  }

  /**
   * Construct a proto for initializing the MODULE$
   * field of the given module.
   */
  def moduleFieldInitializer(clazz: Symbol): P.Statement = {
    val init = P.Statement.newBuilder
    init.setType(P.Statement.StatementType.Expr)
    val initExpr = P.Expr.newBuilder
    initExpr.setType(P.Expr.ExprType.Assignment)
    val assign = P.Assignment.newBuilder
    val lhs = P.Expr.newBuilder
    lhs.setType(P.Expr.ExprType.FieldRef)
    val lhsField = P.FieldRef.newBuilder
    lhsField.setEnclosingType(globalName(clazz, true))
    lhsField.setName("MODULE$")
    lhs.setFieldRef(lhsField)
    assign.setLhs(lhs)
    val rhs = P.Expr.newBuilder
    rhs.setType(P.Expr.ExprType.ThisRef)
    assign.setRhs(rhs)
    initExpr.setAssignment(assign)
    init.setExpr(initExpr)

    init.build
  }

  /**
   * In the constructor for a singleton object, add an
   * initialization of the MODULE$ field.
   */
  def addModuleInitializer(
    bodyProto: P.Statement,
    clazz: Symbol): P.Statement =
  {
    val newBody = P.Statement.newBuilder
    newBody.setType(P.Statement.StatementType.Block)
    val newBlock = P.Block.newBuilder

    val init = moduleFieldInitializer(clazz)

    var initialized = false
    for (stat <- bodyProto.getBlock.getStatementList) {
      newBlock.addStatement(stat)
      if (
        stat.getType == P.Statement.StatementType.Expr && 
        stat.getExpr.getType == P.Expr.ExprType.MethodCall &&
        stat.getExpr.getMethodCall.hasReceiver &&
        stat.getExpr.getMethodCall.getReceiver.getType == P.Expr.ExprType.ThisRef &&
        stat.getExpr.getMethodCall.getSignature.getName == "new"
      ) {
        newBlock.addStatement(init)
        initialized = true
      }
    }
    if (!initialized) newBlock.addStatement(init)

    newBody.setBlock(newBlock)

    newBody.build
  }
}
