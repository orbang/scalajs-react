package japgolly.scalajs.react

import japgolly.scalajs.react.vdom.{TopNode, VdomElement}
import japgolly.scalajs.react.{raw => Raw}
import scala.scalajs.js
import component._
import internal._

object TEMP {

  type Component[N <: TopNode, P <: js.Object, CT[-p, +u] <: CtorType[p, u]] = ComponentRoot[N, P, CT, Unmounted[P]]
  type Unmounted[P <: js.Object]                               = UnmountedRoot[P]
  type Mounted                                                 = Unit

  private type RawComponent[P <: js.Object] = Raw.React.ForwardRefComponent[P]

  private def staticDisplayName = "<ForwardRefComponent>"

  private def rawComponentDisplayName: RawComponent[_ <: js.Object] => String =
    _ => staticDisplayName

  def jsToVdom[P <: js.Object, C <: Children, N <: TopNode](raw: js.Any)(implicit s: CtorType.Summoner[P, C]): Component[N, P, s.CT] = {
    val rc = raw.asInstanceOf[Raw.React.ForwardRefComponent[P]]
    componentRoot[N, P, s.CT, Unmounted[P]](rc, s.pf.rmap(s.summon(rc))(unmountedRoot))(s.pf)
  }

//  def jsToVdom[P <: js.Object, N <: TopNode](raw: js.Any) =
    // JsToVdom[P, N](raw.asInstanceOf[Raw.React.ForwardRefComponent[P]])


//  case class JsToVdom[P <: js.Object, N <: TopNode](raw: Raw.React.ForwardRefComponent[P]) {
//    println(s"raw = $raw")
//
//    // TODO Children
//    def apply(p: P): VdomElement =
//      VdomElement(Raw.React.createElement(raw: Raw.React.ComponentType[P], p))
//
//    def withRef(ref: Ref[N, N])(p: P): VdomElement =
//      VdomElement(Raw.React.createElement(raw: Raw.React.ComponentType[P], p))
//
//    def withRawRef(ref: Raw.React.RefHandle[_])(p: P): VdomElement =
//      VdomElement(Raw.React.createElement(raw: Raw.React.ComponentType[P], p))
//  }

  sealed trait ComponentSimple[N <: TopNode, P, CT[-p, +u] <: CtorType[p, u], U] extends Generic.ComponentSimple[P, CT, U] {
    override final def displayName = rawComponentDisplayName(raw)

    override type Raw <: RawComponent[_ <: js.Object]
    override def cmapCtorProps[P2](f: P2 => P): ComponentSimple[N, P2, CT, U]
    override def mapUnmounted[U2](f: U => U2): ComponentSimple[N, P, CT, U2]
    override def mapCtorType[CT2[-p, +u] <: CtorType[p, u]](f: CT[P, U] => CT2[P, U])(implicit pf: Profunctor[CT2]): ComponentSimple[N, P, CT2, U]

    def withRawRef(ref: Raw.React.RefHandle[N]): Generic.ComponentSimple[P, CT, U]
  }

  // sealed trait RefProvided extends js.Any

  sealed trait ComponentWithRoot[
  N <: TopNode,
      P1, CT1[-p, +u] <: CtorType[p, u], U1,
      P0 <: js.Object, CT0[-p, +u] <: CtorType[p, u], U0]
      extends ComponentSimple[N, P1, CT1, U1] with Generic.ComponentWithRoot[P1, CT1, U1, P0, CT0, U0] {

    override final type Raw = RawComponent[P0]
    override final type Root = ComponentRoot[N, P0, CT0, U0]

    override def cmapCtorProps[P2](f: P2 => P1): ComponentWithRoot[N, P2, CT1, U1, P0, CT0, U0]
    override def mapUnmounted[U2](f: U1 => U2): ComponentWithRoot[N, P1, CT1, U2, P0, CT0, U0]
    override def mapCtorType[CT2[-p, +u] <: CtorType[p, u]](f: CT1[P1, U1] => CT2[P1, U1])(implicit pf: Profunctor[CT2]): ComponentWithRoot[N, P1, CT2, U1, P0, CT0, U0]

    override def withRawRef(ref: Raw.React.RefHandle[N]): Generic.ComponentWithRoot[P1, CT1, U1, P0, CT0, U0]
  }

  final type ComponentRoot[N <: TopNode, P <: js.Object, CT[-p, +u] <: CtorType[p, u], U] =
    ComponentWithRoot[N, P, CT, U, P, CT, U]

  final def componentRoot[N <: TopNode, P <: js.Object, CT[-p, +u] <: CtorType[p, u], U](rc: RawComponent[P], c: CT[P, U])
                                                                          (implicit pf: Profunctor[CT]): ComponentRoot[N, P, CT, U] =
    new ComponentRoot[N, P, CT, U] {
      override def root = this
      override val raw = rc
      override val ctor = c
      override implicit def ctorPF = pf
      override def cmapCtorProps[P2](f: P2 => P) = mappedC(this)(f, identityFn, identityFn, pf)
      override def mapUnmounted[U2](f: U => U2) = mappedC(this)(identityFn, identityFn, f, pf)
      override def mapCtorType[CT2[-p, +u] <: CtorType[p, u]](f: CT[P, U] => CT2[P, U])(implicit pf: Profunctor[CT2]) =
        mappedC(this)(identityFn, f, identityFn, pf)
      override def withRawRef(ref: Raw.React.RefHandle[N]) =
        componentRoot(rc, CtorType.hackBackToSelf(c)(c.withRawProp("ref", ref)))(pf)
    }

  protected final def mappedC[
  N <: TopNode,
  P2, CT2[-p, +u] <: CtorType[p, u], U2,
      P1, CT1[-p, +u] <: CtorType[p, u], U1,
      P0 <: js.Object, CT0[-p, +u] <: CtorType[p, u], U0]
      (from: ComponentWithRoot[N, P1, CT1, U1, P0, CT0, U0])
      (cp: P2 => P1, mc: CT1[P1, U1] => CT2[P1, U1], mu: U1 => U2, pf: Profunctor[CT2])
      : ComponentWithRoot[N, P2, CT2, U2, P0, CT0, U0] =
    new ComponentWithRoot[N, P2, CT2, U2, P0, CT0, U0] {
      override def root = from.root
      override val raw = from.raw
      override val ctor = mc(from.ctor).dimap(cp, mu)
      override implicit def ctorPF = pf
      override def cmapCtorProps[P3](f: P3 => P2) = mappedC(from)(cp compose f, mc, mu, pf)
      override def mapUnmounted[U3](f: U2 => U3) = mappedC(from)(cp, mc, f compose mu, pf)
      override def mapCtorType[CT3[-p, +u] <: CtorType[p, u]](f: CT2[P2, U2] => CT3[P2, U2])(implicit pf3: Profunctor[CT3]) =
        mappedC(this)(identityFn, f, identityFn, pf3)
      override def withRawRef(ref: Raw.React.RefHandle[N]) =
        mappedC(
          componentRoot(from.raw, CtorType.hackBackToSelf(from.ctor)(from.ctor.withRawProp("ref", ref)))(from.ctorPF)
        )(cp, mc, mu, pf)
    }


  // ===================================================================================================================

  sealed trait UnmountedSimple[P, M] extends Generic.UnmountedSimple[P, M] {
    override type Raw <: Raw.React.ComponentElement[_ <: js.Object]
    override final def displayName = staticDisplayName

    override def mapUnmountedProps[P2](f: P => P2): UnmountedSimple[P2, M]
    override def mapMounted[M2](f: M => M2): UnmountedSimple[P, M2]

    override final def renderIntoDOM(container: Raw.ReactDOM.Container, callback: Callback = Callback.empty): Mounted = {
      val result = Raw.ReactDOM.render(raw, container, callback.toJsFn)

      // Protect against future React change.
      assert(result eq null, "Expected rendered functional component to return null; not " + result)

      mountRaw(result)
    }
  }

  sealed trait UnmountedWithRoot[P1, M1, P0 <: js.Object]
    extends UnmountedSimple[P1, M1] with Generic.UnmountedWithRoot[P1, M1, P0, Mounted] {
    override final type Raw = Raw.React.ComponentElement[P0]
    override final type Root = UnmountedRoot[P0]
    override def mapUnmountedProps[P2](f: P1 => P2): UnmountedWithRoot[P2, M1, P0]
    override def mapMounted[M2](f: M1 => M2): UnmountedWithRoot[P1, M2, P0]
  }

  type UnmountedRoot[P <: js.Object] = UnmountedWithRoot[P, Mounted, P]

  private val constUnit: Any => Unit = _ => ()

  def unmountedRoot[P <: js.Object](r: Raw.React.ComponentElement[P]): UnmountedRoot[P] =
    new UnmountedRoot[P] {
      override def mapUnmountedProps[P2](f: P => P2) = mappedU(this)(f, identityFn)
      override def mapMounted[M2](f: Mounted => M2) = mappedU(this)(identityFn, f)

      override def root          = this
      override val raw           = r
      override val mountRaw      = constUnit
      override val vdomElement   = vdom.VdomElement(raw)
      override def key           = jsNullToOption(raw.key)
      override def ref           = None // orNullToOption(raw.ref)
      override def props         = raw.props.asInstanceOf[P]
      override def propsChildren = PropsChildren.fromRawProps(raw.props)
    }

  private def mappedU[P2, M2, P1, M1, P0 <: js.Object](from: UnmountedWithRoot[P1, M1, P0])
                                                      (mp: P1 => P2, mm: M1 => M2): UnmountedWithRoot[P2, M2, P0] =
    new UnmountedWithRoot[P2, M2, P0] {
      override def root          = from.root
      override val raw           = from.raw
      override val mountRaw      = mm compose from.mountRaw
      override def vdomElement   = from.vdomElement
      override def key           = from.key
      override def ref           = from.ref
      override def props         = mp(from.props)
      override def propsChildren = from.propsChildren
      override def mapUnmountedProps[P3](f: P2 => P3) = mappedU(from)(f compose mp, mm)
      override def mapMounted[M3](f: M2 => M3) = mappedU(from)(mp, f compose mm)
    }
}
