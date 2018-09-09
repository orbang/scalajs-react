package japgolly.scalajs.react

import japgolly.scalajs.react.{raw => Raw}
import japgolly.scalajs.react.internal.{identityFn, jsNullToOption}
import scala.scalajs.js
import scala.scalajs.js.|

object Ref {

  def apply[A]: Simple[A] =
    fromJs(Raw.React.createRef[A]())

  def fromJs[A](raw: Raw.React.RefHandle[A]): Simple[A] =
    new Full(raw, identityFn, identityFn)

  def forwardedFromJs[A](f: raw.React.ForwardedRef[A]): Option[Simple[A]] =
    jsNullToOption(f).map(fromJs)

  def react15[A]: Simple[A] = {
    val handle = js.Dynamic.literal("current" -> null).asInstanceOf[Raw.React.RefHandle[A]]
    fromJs(handle)
  }

  type Simple[A] = Full[A, A, A]

  trait Handle[A] {
    val raw: Raw.React.RefHandle[A]
  }

  trait Get[A] { self =>
    val get: CallbackOption[A]

    def map[B](f: A => B): Get[B]

    def widen[B >: A]: Get[B]

    final def foreach(f: A => Unit): Callback =
      foreachCB(a => Callback(f(a)))

    final def foreachCB(f: A => Callback): Callback =
      get.flatMapCB(f).toCallback

    /** Get the reference immediately.
      *
      * ONLY USE THIS IN UNIT TESTS. DO NOT USE THIS IN PRODUCTION CODE.
      *
      * Unsafe for two reasons:
      *
      * 1. It reads an underlying variable. (impurity)
      * 2. It throws an exception when the ref is empty (partiality)
      */
    final def unsafeGet(): A =
      get.asCallback.runNow().getOrElse(sys error "Reference is empty")
  }

  trait Set[A] {
    val set: CallbackKleisli[Option[A], Unit]

    final lazy val rawSetFn: Raw.React.RefFn[A] =
      set.contramap[A | Null](jsNullToOption).toJsFn

    def contramap[B](f: B => A): Set[B]

    def narrow[B <: A]: Set[B]
  }

  trait Fn[I, O] extends Set[I] with Get[O] {
    override def contramap[X](f: X => I): Fn[X, O]
    override def narrow[X <: I]: Fn[X, O]
    override def map[X](f: O => X): Fn[I, X]
    override def widen[X >: O]: Fn[I, X]
  }

  final class Full[I, A, O](val raw: Raw.React.RefHandle[A], l: I => A, r: A => O) extends Handle[A] with Fn[I, O] {

    def root: Simple[A] =
      fromJs(raw)

    override val set: CallbackKleisli[Option[I], Unit] =
      CallbackKleisli((oi: Option[I]) => Callback(raw.current = oi match {
        case Some(i) => l(i)
        case None    => null
      }))

    override val get: CallbackOption[O] =
      CallbackOption(CallbackTo(jsNullToOption(raw.current).map(r)))

    override def contramap[X](f: X => I): Full[X, A, O] =
      new Full(raw, l compose f, r)

    override def narrow[X <: I]: Full[X, A, O] =
      new Full(raw, l, r)

    override def map[X](f: O => X): Full[I, A, X] =
      new Full(raw, l, f compose r)

    override def widen[X >: O]: Full[I, A, X] =
      new Full(raw, l, r)
  }

  // ===================================================================================================================

  final class ToComponent[I, O, C](ref: Fn[I, O], val component: C) extends Fn[I, O] {
    override val get = ref.get
    override val set = ref.set

    override def contramap[A](f: A => I): ToComponent[A, O, C] =
      ToComponent(ref.contramap(f), component)

    override def map[A](f: O => A): ToComponent[I, A, C] =
      ToComponent(ref.map(f), component)

    override def widen[A >: O]: ToComponent[I, A, C] =
      map[A](o => o)

    override def narrow[A <: I]: ToComponent[A, O, C] =
      contramap[A](a => a)
  }

  object ToComponent {

    def apply[I, O, C](ref: Fn[I, O], c: C): ToComponent[I, O, C] =
      new ToComponent[I, O, C](ref, c)

    def inject[I, O, CT[-p, +u] <: CtorType[p, u], P2, U2](c: CT[P2, U2], ref: Fn[I, O]): ToComponent[I, O, CT[P2, U2]] =
      apply(ref, CtorType.hackBackToSelf(c)(c.withRawProp("ref", ref.rawSetFn)))
  }

  // ===================================================================================================================

  type ToJsComponent[F[_], P1, S1, CT1[-p, +u] <: CtorType[p, u], R <: JsComponent.RawMounted[P0, S0], P0 <: js.Object, S0 <: js.Object] =
    Ref.ToComponent[
      JsComponent.RawMounted[P0, S0] with R,
      JsComponent.MountedWithRawType[P0, S0, R],
      CT1[P1, JsComponent.UnmountedMapped[F, P1, S1, R, P0, S0]]]

  def toJsComponent[F[_], P1, S1, CT1[-p, +u] <: CtorType[p, u], R <: JsComponent.RawMounted[P0, S0], P0 <: js.Object, S0 <: js.Object, CT0[-p, +u] <: CtorType[p, u]]
                   (c: JsComponent.ComponentMapped[F, P1, S1, CT1, R, P0, S0, CT0])
                   : ToJsComponent[F, P1, S1, CT1, R, P0, S0] =
    ToComponent.inject(c,
      apply[JsComponent.RawMounted[P0, S0] with R].map(JsComponent.mounted[P0, S0](_).withRawType[R]))

  // ===================================================================================================================

  type ToScalaComponent[P, S, B, CT[-p, +u] <: CtorType[p, u]] =
    Ref.ToComponent[
      ScalaComponent.RawMounted[P, S, B],
      ScalaComponent.MountedImpure[P, S, B],
      CT[P, ScalaComponent.Unmounted[P, S, B]]]

  def toScalaComponent[P, S, B, CT[-p, +u] <: CtorType[p, u]]
                      (c: ScalaComponent[P, S, B, CT])
                      : ToScalaComponent[P, S, B, CT] =
    ToComponent.inject(c,
      apply[ScalaComponent.RawMounted[P, S, B]].map(_.mountedImpure))
}