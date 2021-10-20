package wolfendale.control

sealed class DefaultsTo[A, B]

object DefaultsTo extends LowPriorityDefaultsTo {
  implicit def default[A]: DefaultsTo[A, A] = new DefaultsTo[A, A]
}

trait LowPriorityDefaultsTo {
  implicit def overrideDefault[A, B]: DefaultsTo[A, B] = new DefaultsTo[A, B]
}
