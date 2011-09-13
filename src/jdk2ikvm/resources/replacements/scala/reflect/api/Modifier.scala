package scala.reflect.api

object Modifier {
  
  type Value = Nothing

   val `protected`, `private`, `override`, `abstract`, `final`, 
        `sealed`, `implicit`, `lazy`, `case`, `trait`, 
        deferred, interface, mutable, parameter, covariant, contravariant,
        preSuper, abstractOverride, local, java, static, caseAccessor, 
        defaultParameter, defaultInit, paramAccessor, bynameParameter = sys.error("GWT doesn't support Enumeration")
          
}