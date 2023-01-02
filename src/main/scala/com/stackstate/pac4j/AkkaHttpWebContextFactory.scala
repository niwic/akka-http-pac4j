package com.stackstate.pac4j

import org.pac4j.core.context.{FrameworkParameters, WebContextFactory}

object AkkaHttpWebContextFactory extends WebContextFactory {
  override def newContext(parameters: FrameworkParameters): AkkaHttpWebContext = {
    val akkaHttpFrameworkParameters = parameters.asInstanceOf[AkkaHttpFrameworkParameters]
    lazy val sessionId = akkaHttpFrameworkParameters.changes.sessionId
      .orElse(akkaHttpFrameworkParameters.requestSessionId)

    AkkaHttpWebContext(
      akkaHttpFrameworkParameters.request,
      akkaHttpFrameworkParameters.formFields,
      akkaHttpFrameworkParameters.sessionStorage,
      akkaHttpFrameworkParameters.sessionCookieName,
      akkaHttpFrameworkParameters.changes.copy(sessionId = sessionId),
    )
  }

}
