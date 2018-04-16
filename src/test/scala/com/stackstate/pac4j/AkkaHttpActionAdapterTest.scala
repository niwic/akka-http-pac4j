package com.stackstate.pac4j

import akka.http.scaladsl.model.HttpResponse
import org.scalatest.{ Matchers, WordSpecLike }
import akka.http.scaladsl.model.StatusCodes._
import com.stackstate.pac4j.http.AkkaHttpActionAdapter
import org.scalatest.concurrent.ScalaFutures

class AkkaHttpActionAdapterTest extends WordSpecLike with Matchers with ScalaFutures {
  val dummyContext = new AkkaHttpWebContext
  "AkkaHttpActionAdapter" should {
    "convert 200 to OK" in {
      AkkaHttpActionAdapter.adapt(200, dummyContext).futureValue.response shouldEqual HttpResponse(OK)
    }
    "convert 401 to Unauthorized" in {
      AkkaHttpActionAdapter.adapt(401, dummyContext).futureValue.response shouldEqual HttpResponse(Unauthorized)
    }
    "convert 400 to BadRequest" in {
      AkkaHttpActionAdapter.adapt(400, dummyContext).futureValue.response shouldEqual HttpResponse(BadRequest)
    }
    "convert 201 to Created" in {
      AkkaHttpActionAdapter.adapt(201, dummyContext).futureValue.response shouldEqual HttpResponse(Created)
    }
    "convert 403 to Forbidden" in {
      AkkaHttpActionAdapter.adapt(403, dummyContext).futureValue.response shouldEqual HttpResponse(Forbidden)
    }
    "convert 204 to NoContent" in {
      AkkaHttpActionAdapter.adapt(204, dummyContext).futureValue.response shouldEqual HttpResponse(NoContent)
    }
  }
}