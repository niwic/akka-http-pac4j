package com.stackstate.pac4j.http

import java.util.Optional
import compat.java8.OptionConverters._
import com.stackstate.pac4j.AkkaHttpWebContext
import org.pac4j.core.context.WebContext
import org.pac4j.core.context.session.SessionStore

class AkkaHttpSessionStore() extends SessionStore {
  override def getSessionId(context: WebContext, createSession: Boolean): Optional[String] = {
    internalGetSessionId(context, createSession).asJava
  }

  private def internalGetSessionId(context: WebContext, createSession: Boolean): Option[String] = {
    if (createSession) {
      Some(getOrCreateSessionId(context))
    } else {
      context.asInstanceOf[AkkaHttpWebContext].getSessionId
    }
  }

  def getOrCreateSessionId(context: WebContext): String = context.asInstanceOf[AkkaHttpWebContext].getOrCreateSessionId()

  override def get(context: WebContext, key: String): Optional[Object] =
    context.asInstanceOf[AkkaHttpWebContext].getSessionId match {
      case Some(value) => context.asInstanceOf[AkkaHttpWebContext].sessionStorage.getSessionValue(value, key).asJava
      case None => Optional.empty()
    }

  override def set(context: WebContext, key: String, value: scala.AnyRef): Unit = {
    context.asInstanceOf[AkkaHttpWebContext].sessionStorage.setSessionValue(context.asInstanceOf[AkkaHttpWebContext].getOrCreateSessionId(), key, value)
    ()
  }

  override def destroySession(context: WebContext): Boolean = context.asInstanceOf[AkkaHttpWebContext].destroySession()

  override def getTrackableSession(context: WebContext): Optional[AnyRef] =
    context.asInstanceOf[AkkaHttpWebContext].getSessionId.asInstanceOf[Option[AnyRef]].asJava

  override def buildFromTrackableSession(context: WebContext, trackableSession: scala.Any): Optional[SessionStore] = {
    trackableSession match {
      case session: String if session.nonEmpty =>
        context.asInstanceOf[AkkaHttpWebContext].trackSession(session)
        Optional.of(this)

      case _ =>
        Optional.empty()
    }
  }

  override def renewSession(context: WebContext): Boolean = {
    internalGetSessionId(context, createSession = false).foreach { sessionId =>
      val sessionValues = context.asInstanceOf[AkkaHttpWebContext].sessionStorage.getSessionValues(sessionId)
      destroySession(context)
      val newSessionId = getOrCreateSessionId(context)
      sessionValues.foreach(context.asInstanceOf[AkkaHttpWebContext].sessionStorage.setSessionValues(newSessionId, _))
    }
    true
  }
}
