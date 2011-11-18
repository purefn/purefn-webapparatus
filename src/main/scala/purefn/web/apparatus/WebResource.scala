package purefn.web
package apparatus

import scalaz._, Scalaz._, effect._, scalaz.iteratee._

import Web._

object WebResource extends WebResources

trait WebResources {
  type WebResourceIter[A] = IterateeT[Throwable, String, IO, A]
  type InitFn[S, C] = ReaderT[S, WebState, C]
  type WebResourceFn[C, A] = StateT[(C, WebData), WebResourceIter, A]
  type BodyProducingFn[C] = WebResourceFn[C, ResponseBody]
  type ContentTypesProvidedFn[C] = WebResourceFn[C, NonEmptyList[(String, BodyProducingFn[C])]]

  implicit def provideContentTypes[C](ts: NonEmptyList[(String, BodyProducingFn[C])]): ContentTypesProvidedFn[C] = 
    stateT(s => (ts, s).point[WebResourceIter])

  implicit def provideContentType[C](t: (String, BodyProducingFn[C])): ContentTypesProvidedFn[C] =
    provideContentTypes(nels(t))

  def initIdent[S]: InitFn[S, S] = kleisli(s => s.point[WebState])
    
  def webResource[S, C](
      init: InitFn[S, C] = initIdent[S],
      contentTypesProvided: ContentTypesProvidedFn[C]): WebFn[S, Unit] = {

    type WebResourceFnC[A] = WebResourceFn[C, A]

    def pickBodyProducingFn(cts: NonEmptyList[(String, BodyProducingFn[C])]): 
        WebResourceFnC[Validation[Response, (String, BodyProducingFn[C])]] = {
      def findBestMatch(accepts: NonEmptyList[String]): Validation[Response, (String, BodyProducingFn[C])] = 
        // TODO some algorithm to parse accepts and find the best type - for now, just use text/html
        cts.list.find(ct => ct._1 === "text/html").toSuccess(fourOhSix)
        
      getRequest[C] map 
        getHeaders[Request]("Accept") map 
        (_ map (findBestMatch) getOrElse (cts.head.success))
    }
    
    def runBodyProducingFn(fn: (String, BodyProducingFn[C])): WebResourceFnC[Unit] = fn._2 flatMap (enum =>
      modifyResponse(
        modifyResponseBody(new (ResponseEnumT ~> ResponseEnumT) {
          def apply[A](e: ResponseEnumT[A]) = e |+| enum[A]
        }) andThen 
        setHeader[Response]("Content-Type", fn._1)
      ))
    
    def addBodyToResponse(b: ResponseBody): WebResourceFnC[Unit] =
      modifyResponse(modifyResponseBody(new (ResponseEnumT ~> ResponseEnumT) {
        def apply[A](e: ResponseEnumT[A]) = e |+| b[A]
      }))
      
    def stateMachine: WebResourceFnC[Validation[Response, Unit]] =
      for {
        contentTypes <- contentTypesProvided
        bodyFn       <- pickBodyProducingFn(contentTypes)
        result       <- (bodyFn map runBodyProducingFn).sequence[WebResourceFnC, Unit]
      } yield result
      
    def dropContext(r: (Validation[Response, Unit], (C, WebData))) = (some(r._1), r._2._2)

    kleisli(s => Web(stateT[WebData, WebIter, Option[Validation[Response, Unit]]](wd => 
      init(s) runT(wd) flatMap(s => 
        stateMachine runT(s) map dropContext))))
  }
  
  /* WebResourceFn versions of get, modify, and put */
  def get[C]: WebResourceFn[C, (C, WebData)] = getT[(C, WebData), WebResourceIter]

  def put[C](cwd: => (C, WebData)): WebResourceFn[C, Unit] = putT[(C, WebData), WebResourceIter](cwd)
  
  def modify[C](f: (C, WebData) => (C, WebData)): WebResourceFn[C, Unit] =
    modifyT[(C, WebData), WebResourceIter](cw => f(cw._1, cw._2))

  /* Request handling functions */
  def getRequest[C]: WebResourceFn[C, Request] = get map (_._2.request)

  def putRequest[C](request: Request): WebResourceFn[C, Unit] = modify((c, w) => (c, w.copy(request = request)))

  def modifyRequest[C](f: Request => Request): WebResourceFn[C, Unit] = modify((c, w) => (c, w.copy(request = f(w.request))))

  /* Response handling functions */
  def getResponse[C]: WebResourceFn[C, Response] = get map (_._2.response)

  def putResponse[C](response: Response): WebResourceFn[C, Unit] = modify((c, w) => (c, w.copy(response = response)))

  def modifyResponse[C](f: Response => Response): WebResourceFn[C, Unit] = 
    modify((c, w) => (c, w.copy(response = f(w.response))))
    
  /* Request context handling functions */
  def getContext[C]: WebResourceFn[C, C] = get map(_._1)
  
  def putContext[C](c: C): WebResourceFn[C, Unit] = modify((_, w) => (c, w))
  
  def modifyContext[C](f: C => C): WebResourceFn[C, Unit] = modify((c, w) => (f(c), w))
  
  def fourOhSix: Response = 
    Response(
      status = 406,
      statusReason = "Not Acceptable",
      contentLength = some(0)
    )
}
