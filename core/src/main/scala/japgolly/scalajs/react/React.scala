package japgolly.scalajs.react

import japgolly.scalajs.react.internal.{JsRepr, NotAllowed}
import japgolly.scalajs.react.vdom.{TopNode, VdomElement, VdomNode}
import japgolly.scalajs.react.{raw => Raw}

object React {
  def raw = Raw.React

  /** Create a new context.
    *
    * If you'd like to retain type information about the JS type used under-the-hood with React,
    * use `React.Context(defaultValue)` instead.
    *
    * @since 1.3.0 / React 16.3.0
    */
  def createContext[A](defaultValue: A)(implicit jsRepr: JsRepr[A]): Context[A] =
    Context(defaultValue)(jsRepr)

  @deprecated("Use Ref. For details see https://github.com/japgolly/scalajs-react/blob/master/doc/REFS.md", "1.3.0 / React 16.3.0")
  def createRef(notAllowed: NotAllowed) = NotAllowed.body

  type Context[A] = feature.Context[A]
  val Context     = feature.Context

  val Fragment    = feature.ReactFragment

  /** Ref forwarding is an opt-in feature that lets some components take a ref they receive,
    * and pass it further down (in other words, "forward" it) to a child.
    *
    * See https://reactjs.org/docs/forwarding-refs.html
    */
  object forwardRef {

//    def apply[P, R](f: (P, Option[ref3.Ref.Handle[R]]) => VdomNode) =
//      ref3.TEMP.fromRaw()

//    // TODO vary I/O with variance
//    def toVdom[P, N <: TopNode](f: (P, Option[Ref[N, N]]) => VdomNode) =
//      ???
//
//    def toScalaComponent[P, S, B, CT[-p, +u] <: CtorType[p, u]](c: ScalaComponent[P, S, B, CT]) = new {
//      def apply[P2](f: (P2, Option[Ref.ToScalaComponent[P, S, B, CT]]) => VdomNode) =
//        ???
//    }
  }


  /** StrictMode is a tool for highlighting potential problems in an application.
    * Like Fragment, StrictMode does not render any visible UI.
    * It activates additional checks and warnings for its descendants.
    *
    * Strict mode checks are run in development mode only; they do not impact the production build.
    *
    * @since 1.3.0 / React 16.3.0
    */
  def StrictMode(ns: VdomNode*): VdomElement =
    VdomElement(Raw.React.createElement(Raw.React.StrictMode, null, ns.map(_.rawNode): _*))
}
