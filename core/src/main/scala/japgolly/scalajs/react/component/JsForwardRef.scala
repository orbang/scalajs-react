package japgolly.scalajs.react.component

import japgolly.scalajs.react.{raw => Raw, _}
import japgolly.scalajs.react.internal._
import scala.scalajs.js

object JsForwardRef {

  type Component[R, P <: js.Object, CT[-p, +u] <: CtorType[p, u]] = ComponentRoot[R, P, CT, Unmounted[P]]
  type Unmounted[P <: js.Object]                                  = UnmountedRoot[P]
  type Mounted                                                    = Unit

  def apply[R, P <: js.Object, C <: Children]
           (raw: js.Any)
           (implicit s: CtorType.Summoner[P, C], where: sourcecode.FullName, line: sourcecode.Line): Component[R, P, s.CT] = {
    InspectRaw.assertValidJsForwardRefComponent(raw, where, line)
    force[R, P, C](raw)(s)
  }

  def force[R, P <: js.Object, C <: Children](raw: js.Any)(implicit s: CtorType.Summoner[P, C]): Component[R, P, s.CT] = {
    val rc = raw.asInstanceOf[Raw.React.ForwardRefComponent[P, R]]
    componentRoot[R, P, s.CT, Unmounted[P]](rc, s.pf.rmap(s.summon(rc))(unmountedRoot))(s.pf)
  }

  def fromRaw[R, P <: js.Object, C <: Children](r: Raw.React.ForwardRefComponent[P, R])(implicit s: CtorType.Summoner[P, C]): Component[R, P, s.CT] =
    force[R, P, C](r)(s)

  // ===================================================================================================================

  // TODO Allow mapping of R

  private def staticDisplayName = "<ForwardRefComponent>"

  private def rawComponentDisplayName: Raw.React.ForwardRefComponent[_ <: js.Object, _] => String =
    _ => staticDisplayName

  sealed trait ComponentSimple[R, P, CT[-p, +u] <: CtorType[p, u], U] extends Generic.ComponentSimple[P, CT, U] {
    override final def displayName = rawComponentDisplayName(raw)

    override type Raw <: Raw.React.ForwardRefComponent[_ <: js.Object, R]
    override def cmapCtorProps[P2](f: P2 => P): ComponentSimple[R, P2, CT, U]
    override def mapUnmounted[U2](f: U => U2): ComponentSimple[R, P, CT, U2]
    override def mapCtorType[CT2[-p, +u] <: CtorType[p, u]](f: CT[P, U] => CT2[P, U])(implicit pf: Profunctor[CT2]): ComponentSimple[R, P, CT2, U]

    def withRef[RR <: R](ref: Ref.Handle[RR]): Generic.ComponentSimple[P, CT, U]
  }

  sealed trait ComponentWithRoot[R,
      P1, CT1[-p, +u] <: CtorType[p, u], U1,
      P0 <: js.Object, CT0[-p, +u] <: CtorType[p, u], U0]
      extends ComponentSimple[R, P1, CT1, U1] with Generic.ComponentWithRoot[P1, CT1, U1, P0, CT0, U0] {

    override final type Raw = Raw.React.ForwardRefComponent[P0, R]
    override final type Root = ComponentRoot[R, P0, CT0, U0]

    override def cmapCtorProps[P2](f: P2 => P1): ComponentWithRoot[R, P2, CT1, U1, P0, CT0, U0]
    override def mapUnmounted[U2](f: U1 => U2): ComponentWithRoot[R, P1, CT1, U2, P0, CT0, U0]
    override def mapCtorType[CT2[-p, +u] <: CtorType[p, u]](f: CT1[P1, U1] => CT2[P1, U1])(implicit pf: Profunctor[CT2]): ComponentWithRoot[R, P1, CT2, U1, P0, CT0, U0]

    override def withRef[RR <: R](ref: Ref.Handle[RR]): Generic.ComponentWithRoot[P1, CT1, U1, P0, CT0, U0]
  }

  final type ComponentRoot[R, P <: js.Object, CT[-p, +u] <: CtorType[p, u], U] =
    ComponentWithRoot[R, P, CT, U, P, CT, U]

  final def componentRoot[R, P <: js.Object, CT[-p, +u] <: CtorType[p, u], U](rc: Raw.React.ForwardRefComponent[P, R], c: CT[P, U])
                                                                             (implicit pf: Profunctor[CT]): ComponentRoot[R, P, CT, U] =
    new ComponentRoot[R, P, CT, U] {
      override def root = this
      override val raw = rc
      override val ctor = c
      override implicit def ctorPF = pf
      override def cmapCtorProps[P2](f: P2 => P) = mappedC(this)(f, identityFn, identityFn, pf)
      override def mapUnmounted[U2](f: U => U2) = mappedC(this)(identityFn, identityFn, f, pf)
      override def mapCtorType[CT2[-p, +u] <: CtorType[p, u]](f: CT[P, U] => CT2[P, U])(implicit pf: Profunctor[CT2]) =
        mappedC(this)(identityFn, f, identityFn, pf)

      override def withRef[RR <: R](ref: Ref.Handle[RR]) =
        componentRoot(rc, setRef(c, ref))(pf)
    }

  private def setRef[CT[-p, +u] <: CtorType[p, u], P, U](c: CT[P, U], ref: Ref.Handle[_]): CT[P, U] =
    CtorType.hackBackToSelf(c)(c.withRawProp("ref", ref.raw))

  protected final def mappedC[R,
      P2, CT2[-p, +u] <: CtorType[p, u], U2,
      P1, CT1[-p, +u] <: CtorType[p, u], U1,
      P0 <: js.Object, CT0[-p, +u] <: CtorType[p, u], U0]
      (from: ComponentWithRoot[R, P1, CT1, U1, P0, CT0, U0])
      (cp: P2 => P1, mc: CT1[P1, U1] => CT2[P1, U1], mu: U1 => U2, pf: Profunctor[CT2])
      : ComponentWithRoot[R, P2, CT2, U2, P0, CT0, U0] =
    new ComponentWithRoot[R, P2, CT2, U2, P0, CT0, U0] {
      override def root = from.root
      override val raw = from.raw
      override val ctor = mc(from.ctor).dimap(cp, mu)
      override implicit def ctorPF = pf
      override def cmapCtorProps[P3](f: P3 => P2) = mappedC(from)(cp compose f, mc, mu, pf)
      override def mapUnmounted[U3](f: U2 => U3) = mappedC(from)(cp, mc, f compose mu, pf)
      override def mapCtorType[CT3[-p, +u] <: CtorType[p, u]](f: CT2[P2, U2] => CT3[P2, U2])(implicit pf3: Profunctor[CT3]) =
        mappedC(this)(identityFn, f, identityFn, pf3)

      override def withRef[RR <: R](ref: Ref.Handle[RR]) =
        from.withRef(ref).mapCtorType(mc)(pf).mapUnmounted(mu).cmapCtorProps(cp)
    }


  // ===================================================================================================================

  // TODO Copy-pasted ↓
  // TODO Add R - we know the ref type

  sealed trait UnmountedSimple[P, M] extends Generic.UnmountedSimple[P, M] {
    override type Raw <: Raw.React.ComponentElement[_ <: js.Object]
    override final def displayName = staticDisplayName

    override def mapUnmountedProps[P2](f: P => P2): UnmountedSimple[P2, M]
    override def mapMounted[M2](f: M => M2): UnmountedSimple[P, M2]

    override final def renderIntoDOM(container: Raw.ReactDOM.Container, callback: Callback = Callback.empty): Mounted = {
      val result = Raw.ReactDOM.render(raw, container, callback.toJsFn)

      // Protect against future React change.
      assert(result eq null, s"Expected rendered $displayName to return null; not $result")

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
      override def ref           = jsNullToOption(raw.ref)
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