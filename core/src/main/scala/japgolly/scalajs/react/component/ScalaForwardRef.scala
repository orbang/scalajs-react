package japgolly.scalajs.react.component

import japgolly.scalajs.react.internal._
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.{Children, CtorType, PropsChildren, Ref, raw}
import scala.scalajs.js

object ScalaForwardRef {

  type Component[P, R, CT[-p, +u] <: CtorType[p, u]] = JsForwardRef.ComponentWithRoot[P, R, CT, Unmounted[P], Box[P], CT, JsForwardRef.Unmounted[Box[P]]]
  type Unmounted[P]                                  = JsForwardRef.UnmountedWithRoot[P, Mounted, Box[P]]
  type Mounted                                       = JsForwardRef.Mounted

  private def create[P, R, C <: Children, CT[-p, +u] <: CtorType[p, u]]
      (render: (Box[P] with raw.PropsWithChildren, Option[Ref.Simple[R]]) => VdomElement)
      (implicit s: CtorType.Summoner.Aux[Box[P], C, CT]): Component[P, R, CT] = {

    val jsRender: js.Function2[Box[P] with raw.PropsWithChildren, raw.React.ForwardedRef[R], raw.React.Node] =
      (p: Box[P] with raw.PropsWithChildren, r: raw.React.ForwardedRef[R]) =>
        render(p, Ref.forwardedFromJs(r)).rawNode

    val rawComponent = raw.React.forwardRef(jsRender)

    JsForwardRef.force[Box[P], C, R](rawComponent)(s)
      .cmapCtorProps[P](Box(_))
      .mapUnmounted(_.mapUnmountedProps(_.unbox))
  }

  def apply[R](render: Option[Ref.Simple[R]] => VdomElement): Component[Unit, R, CtorType.Nullary] =
    create((_, r) => render(r))

  def apply[P, R](render: (P, Option[Ref.Simple[R]]) => VdomElement): Component[P, R, CtorType.Props] =
    create((p, r) => render(p.unbox, r))

  def withChildren[P, R](render: (P, PropsChildren, Option[Ref.Simple[R]]) => VdomElement): Component[P, R, CtorType.PropsAndChildren] =
    create((b, r) => render(b.unbox, PropsChildren(b.children), r))

  def justChildren[R](render: (PropsChildren, Option[Ref.Simple[R]]) => VdomElement): Component[Unit, R, CtorType.Children] =
    create((b, r) => render(PropsChildren(b.children), r))
}
