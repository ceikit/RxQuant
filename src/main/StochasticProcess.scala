package main

import main.types._

import scala.util.{Success, Try}
// d x(t) = a(t,x(t))*dt + b(t,x(t))*dW(t)


abstract class StochasticProcess{

    // current value of the process
    var x_t : Try[Double]

    // drift a(t,x(t))
    def drift(t: Time, x: Try[Double]): Try[Double]

    // diffusion b(t,x(t))
    def sigma(t: Time, x: Try[Double]): Try[Double]

    // discretization used to make the process evolve
    def discretization: Scheme

    // returns x(t) + dx(t) --> uses scheme discretization
    def evolve (t: Time, dt: Time, dw: Double): Try[Double] = discretization.apply(this, t, dt, dw)

  }

// d x(t) = mu * x(t) *dt + sigma * x(t) *dW(t)


 class GeometricBrownian(mu: Double, sigma: Double, initialValue: Try[Double], scheme: Scheme) extends StochasticProcess {

    // current value of the process
    var x_t : Try[Double] = initialValue

    // drift mu(t,x(t))
    def drift(t: Time, x: Try[Double]): Try[Double] =  Success(mu * x.get)

    // diffusion sigma(t,x(t))
    def sigma(t: Time, x: Try[Double]): Try[Double] = Success(sigma * x.get)

    // discretization used to make the process evolve
    def discretization = scheme

    // returns x(t) + dx(t) --> uses scheme discretization
    override def evolve (t: Time, dt: Time, dw: Double): Try[Double] = discretization.apply(this, t, dt, dw)
}


// d x(t) = k(thetha - x_t )*dt + sigma * sqrt(x_t) *dW(t)

class CIR ( k: Double, thet: Double, sig: Double, initialValue: Try[Double], scheme: Scheme ) extends StochasticProcess {

  // current value of the process
  var x_t: Try[Double]= initialValue

  var kappa: Double = k

  var theta: Double = thet

  var sigma: Double = sig

  // drift mu(t,x(t))
  def drift(t: Time, x: Try[Double]): Try[Double] = Success( k * (thet - x.get) )

  // diffusion sigma(t,x(t))
  def sigma(t: Time, x: Try[Double]): Try[Double] = Success( sig * math.sqrt(x.get) )

  // discretization used to make the process evolve
  def discretization = scheme

  // returns x(t) + dx(t) --> uses scheme discretization
  override def evolve(t: Time, dt: Time, dw: Double): Try[Double] = discretization.apply(this, t, dt, dw)
}


// d x(t) = mu * x(t) *dt + sigma * x(t) *dW(t)


class Heston( m: Double, rh: Double, k: Double, thet: Double, sig: Double, varia: (Double, Double), initialValue: Try[Double], scheme: Scheme ) extends StochasticProcess {

  // current value of the process
  var x_t : Try[Double] = initialValue

  var mu = m ; var kappa: Double = k ; var theta: Double = thet ; var sigm: Double = sig; var rho = rh

  var variance = varia // ( V(t-1), V(t) ) : previous and current value of the variance

  // drift mu(t,x(t))
  def drift(t: Time, x: Try[Double]): Try[Double] =  Success(mu * x.get)

  // diffusion sigma(t,x(t))
  def sigma(t: Time, x: Try[Double]): Try[Double] =
    for {
      x <- x
      variaCurrent = variance._2
    } yield math.sqrt(variaCurrent) * x

  // discretization used to make the process evolve
  def discretization = scheme

  // returns x(t) + dx(t) --> uses scheme discretization
  override def evolve (t: Time, dt: Time, dw: Double): Try[Double] = discretization.apply(this, t, dt, dw)
}
