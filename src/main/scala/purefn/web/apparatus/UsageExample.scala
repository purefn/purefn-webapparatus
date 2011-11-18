package purefn.web
package apparatus

import scalaz._, Scalaz._, scalaz.iteratee._, effect._

sealed trait Config {
  val name: String
}

object UsageExample {
  import Web._, purefn.web.apparatus.{WebResource => wr}, wr.{getRequest => _, _}
    
  type AppWebFn[A] = WebFn[Config, A]
   
  def hello: AppWebFn[Unit] = {
    case class Say(greeting: String)
    
    def init: InitFn[Config, Say] = (c: Config) => Say("Hello, world! From " + c.name).point[WebState]
    
    def toHtml: BodyProducingFn[Say] = for {
      c <- wr.getContext
      r <- wr.getRequest
    } yield r.contextPath + r.pathInfo + "> " + r.params.get("greeting").getOrElse(c.greeting)
    
    webResource(init = init, contentTypesProvided = "text/html" -> toHtml)
  }
 
  def fibonacci: AppWebFn[Unit] = {
    
    def init: InitFn[Config, Unit] = (c: Config) => ().point[WebState]
    lazy val fib: Stream[Long] = Stream.cons(1, Stream.cons(2, fib.zip(fib.tail).map(x => x._1 + x._2)))
    
    def toHtml: BodyProducingFn[Unit] = for {
      r <- wr.getRequest
    } yield { 
      val a = r.params.get("n").flatMap(_.headOption).map(_.parseInt.fold(_.getMessage, fib(_).shows)).getOrElse("missing parameter")
      a
    }
    
    webResource(init = init, contentTypesProvided = "text/html" -> toHtml)
  }

  val helloPt= pathLit("hello") /: pathParam[String]("greeting") /: **
  val fibPt= pathLit("fib") /: pathParam[Int]("n") /: **
  val router = route(
      (**, hello)
    , (pathLit("hello"), hello)
    , (helloPt, hello)
    , (fibPt, fibonacci)
  )

  println(helloPt(() :: "and stuff" :: List("dudes and dudettes") :: HNil))
   
  def main(args: Array[String]): Unit = mainIO(args) unsafePerformIO 
  
  def mainIO(args: Array[String]): IO[Unit] = {
    val config = new Config { val name = "Usage Example!" }
    val request = Request(
      pathInfo = "hello",
      headers = Map(CaseInsensitive("Accept") -> nels("text/html")))

    router(config).run(request).runT(throwIO(_)) flatMap  (rr =>
      // write response to servlet response
      putStrLn(rr._1.shows) >|>
        putStrLn(rr._2.shows) >|>
        putStrLn("Response body") >|>
        putStrLn("=====================================") >|>
        ((putStrTo[Throwable, String](System.out) >>== rr._2.body[Unit]) runT(throwIO(_))) >|>
        putStrLn("") >|>
        putStrLn("=====================================")
      )
  }
}

