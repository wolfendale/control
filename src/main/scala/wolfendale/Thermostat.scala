package wolfendale

final case class Kelvin(value: Double) extends AnyVal {

  def +(other: Double): Kelvin =
    Kelvin(value + other)

  def -(other: Double): Kelvin =
    Kelvin(value - other)
}

final case class Celsius(value: Double) extends AnyVal

final case class Thermostat[A](currentTemp: Kelvin, getTemp: Kelvin => A) {

  def extract: A = getTemp(currentTemp)

  def extend[B](f: Thermostat[A] => B): Thermostat[B] =
    Thermostat(currentTemp, (k: Kelvin) => f(Thermostat(k, getTemp)))
}
