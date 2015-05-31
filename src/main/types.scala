package main

import rx.lang.scala.Observable

import scala.util.Try


object types {
  type Time = Double
  type Scheme = (StochasticProcess, Time, Time, Double) => Try[Double]
  type TimeSeries = Observable[Double]
  //type ReactiveScheme = ( (ReactiveStochasticProcess, Time, Time, Double) => Try[Double] )

}
