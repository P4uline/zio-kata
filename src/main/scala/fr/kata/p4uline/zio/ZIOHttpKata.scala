package fr.kata.p4uline.zio

import zio._
import zhttp.http._
import zhttp.http.Http
import zhttp.service.Server

object ZIOHttpKata extends ZIOAppDefault {

  val port = 9000

  val app: Http[Any, Nothing, Request, Response] = Http.collect[Request] { // partial function
    // !! is the root path
    case Method.GET -> !! / "owls" => Response.text("Hello, owls!")
  }

  val zApp: UHttpApp = Http.collectZIO[Request] {
    case Method.POST -> !! / "owls" => Random.nextIntBetween(3, 5).map(n => Response.text("Hello " * n + ", owls!"))
  }

  val combined = app ++ zApp


  // middelware
  val wrapped = combined @@ Middleware.debug
  // request -> middleware -> combined
  val loggingHttp = combined @@ Verbose.log

  val httpProgram = for {
    _ <- Console.printLine(s"Stating server at http://localhost:$port")
    _ <- Server.start(port, loggingHttp)
  } yield ()
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    httpProgram
}
// request => contramap => http application => response => map => final response
//         | -------------------- Middleware --------------------|
object Verbose {
  def log[R, E >: Exception]: Middleware[R, E, Request, Response, Request, Response] = new Middleware[R, E, Request, Response, Request, Response] {

    override def apply[R1 <: R, E1 >: E](http: Http[R1, E1, Request, Response]): Http[R1, E1, Request, Response] =
      http
        .contramapZIO[R1, E1, Request] { request =>
          for {
            _ <- Console.printLine(s"> ${request.method} ${request.path} ${request.version}")
            _ <- ZIO.foreach(request.headers.toList) { header =>
              Console.printLine(s"> ${header._1} ${header._2}")
            }
          } yield request
        }
        .mapZIO[R1, E1, Response]( r =>
        for {
          _ <- Console.printLine(s"< ${r.status}")
          _ <- ZIO.foreach(r.headers.toList) { header =>
            Console.printLine(s"< ${header._1} ${header._2}")
          }
        }  yield r
      )
  }
}
