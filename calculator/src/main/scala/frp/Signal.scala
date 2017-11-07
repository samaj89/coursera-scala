package frp

import scala.util.DynamicVariable

/*
 * Each signal maintains:
 *   - its current value
 *   - the current expression the defines the signal value
 *   - a set of observers, the others signals that depend on its value
 *
 * If the signal changes, all its dependent signals are reevaluated
 */

class Signal[T](expr: => T) {
  import Signal._
  private var myExpr: () => T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()
  update(expr)

  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  // Computes a signal's new value and propagates the change to dependent signals
  protected def computeValue(): Unit = {
    val newValue = caller.withValue(this)(myExpr())
    if (myValue != newValue) {
      myValue = newValue
      // clear the set of observers and reevaluate all of their values
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }

  def apply(): T = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    myValue
  }

}

object Signal {
  // Global state - the worst kind of state!!! Parallel processing => race conditions
  //private val caller = new StackableVariable[Signal[_]](NoSignal)

  // Thread-local state - each thread accesses its own copy of the state
  // Disadvantages:
  //   (1) imperative nature produces hidden dependencies
  //   (2) not too efficient as it uses a global hash table lookup
  //   (3) performs poorly in situations with multiplexed threads
  private val caller = new DynamicVariable[Signal[_]](NoSignal)
  def apply[T](expr: => T) = new Signal(expr)
}

class Var[T](expr: => T) extends Signal[T](expr) {
  override def update(expr: => T): Unit = super.update(expr)
}

object Var {
  def apply[T](expr: => T) = new Var(expr)
}

// A 'sentinel' object to use on initialization, when there is no caller
object NoSignal extends Signal[Nothing](???) {
  override def computeValue() = ()
}