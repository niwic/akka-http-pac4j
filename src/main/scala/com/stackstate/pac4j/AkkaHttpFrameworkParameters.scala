package com.stackstate.pac4j

import akka.http.scaladsl.model.HttpRequest
import com.stackstate.pac4j.AkkaHttpWebContext.ResponseChanges
import com.stackstate.pac4j.store.SessionStorage
import org.pac4j.core.context.FrameworkParameters

case class AkkaHttpFrameworkParameters(request: HttpRequest,
                                       formFields: Seq[(String, String)],
                                       sessionStorage: SessionStorage,
                                       sessionCookieName: String,
                                       changes: ResponseChanges = ResponseChanges.empty) extends FrameworkParameters {
  def requestSessionId: Option[String] =
    request.cookies
    .find(_.name == sessionCookieName)
    .map(_.value)
    .filter(_.nonEmpty)
    .find(session => sessionStorage.sessionExists(session))

  def getServerName: String = {
    request.getUri().host.address().split(":")(0)
  }
}
