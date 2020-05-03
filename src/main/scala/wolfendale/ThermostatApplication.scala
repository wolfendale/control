package wolfendale

object ThermostatApplication extends App {

  def kelvinToCelsius(k: Kelvin): Celsius = Celsius(k.value - 273.15)

  val initialThermostat = Thermostat(Kelvin(298.15), kelvinToCelsius)

  // wrong but interesting!
  // composing the function is basically extend? No? But interesting? Map?
//  def up[A](thermostat: Thermostat[A]): Thermostat[A] =
//    Thermostat(thermostat.currentTemp, thermostat.getTemp compose (i => Kelvin(i.value + 1)))
//  def down[A](thermostat: Thermostat[A]): Thermostat[A] =
//    Thermostat(thermostat.currentTemp, thermostat.getTemp compose (i => Kelvin(i.value - 1)))

  def up[A](thermostat: Thermostat[A]): A =
    thermostat.getTemp(thermostat.currentTemp + 1)
  def down[A](thermostat: Thermostat[A]): A =
    thermostat.getTemp(thermostat.currentTemp - 1)

  def upP[A](thermostat: Thermostat[A]): Thermostat[A] =
    Thermostat(thermostat.currentTemp + 1, thermostat.getTemp)
  def downP[A](thermostat: Thermostat[A]): Thermostat[A] =
    Thermostat(thermostat.currentTemp - 1, thermostat.getTemp)

  def string(thermostat: Thermostat[Celsius]): String =
    s"${thermostat.extract.value} Celsius"

  println("extract the initial value")
  println(initialThermostat.extract)

  println()
  println("up/down")
  println(up(initialThermostat))
  println(down(initialThermostat))

  println()
  println("string")
  println(string(initialThermostat))

  println()
  println("chaining")
  println(string(upP(upP(initialThermostat))))

  println()
  println("should be the same")
  println(string(upP(initialThermostat)))
  println(string(initialThermostat.extend(up)))
}
