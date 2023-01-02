package com.stackstate.pac4j.http

import com.stackstate.pac4j.{AkkaHttpFrameworkParameters, AkkaHttpWebContextFactory}
import org.pac4j.core.context.FrameworkParameters
import org.pac4j.core.context.session.SessionStoreFactory

object AkkaHttpSessionStoreFactory extends SessionStoreFactory {
  override def newSessionStore(parameters: FrameworkParameters): AkkaHttpSessionStore = {
    val akkaHttpFrameworkParameters = parameters.asInstanceOf[AkkaHttpFrameworkParameters]
    val akkaContext = AkkaHttpWebContextFactory.newContext(akkaHttpFrameworkParameters)
    akkaContext.getSessionStore
  }

}
