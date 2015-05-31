package main

import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.chart.{XYChart, LineChart, NumberAxis}
import javafx.stage.Stage

import main.types._

import scala.util.{Success, Try}


class LineChartStochasticProcess extends Application {

  //def values: List[Try[Double]]

  override def start(stage: Stage): Unit = {
    //val x: List[Try[Double]] = values

    stage.setTitle("Line Chart Sample")
    //defining the axes
    val xAxis = new NumberAxis()
    val yAxis = new NumberAxis()
    xAxis.setLabel("Number of Month")

    //creating the chart
    val lineChart = new LineChart[Number, Number](xAxis, yAxis)

    lineChart.setTitle("QuantCeikit")
    //defining a series
    val series = new XYChart.Series[Number, Number]()
    series.setName("Ceikit: Geometric Brownian Motion")

    //populating the series with data from x
    //val time = List.range(1, x.length)

    val result = getResults()

    for {
      i <- List.range(0, result.length)
    } yield series.getData.add(new XYChart.Data(i, result(i).get))


    val scene = new Scene(lineChart, 800, 600)
    lineChart.getData.add(series)


    stage.setScene(scene)
    stage.show()
  }

  def getResults(): List[Try[Double]] = {
    val t0: Time = 0 // initial time
    val S0: Try[Double] = Success(10) // initial value of the stock
    val N = 1000 // number of time-steps
    val T: Time = 10 // maturity
    val dt = (T - t0) / N // time-step length

    // random number generator
    //val dw = math.sqrt(dt) * Random.nextGaussian()
    val ceikit = new GeometricBrownian(0.1, 0.5, S0, Discretization.milstein)
    S0 :: Evolution.run(ceikit, t0, T, dt)
  }


  def getResults2(): List[Try[Double]] = {
    val t0: Time = 0 // initial time
    val S0: Try[Double] = Success(0.04) // initial value of the stock
    val N = 1000 // number of time-steps
    val T: Time = 12 // maturity
    val dt = (T - t0) / N // time-step length

    // random number generator
    //val dw = math.sqrt(dt) * Random.nextGaussian()
    val ceikit = new GeometricBrownian(0.1, 0.5, S0, Discretization.euler)
    S0 :: Evolution.run(ceikit, t0, T, dt)
  }

  def getResults3(): List[Try[Double]] = {
    val t0: Time = 0 // initial time
    val S0: Try[Double] = Success(0.04) // initial value of the stock
    val N = 1000 // number of time-steps
    val T: Time = 100 // maturity
    val dt = (T - t0) / N // time-step length


    val ceikit = new CIR(10, 0.04, 0.5, S0, Discretization.varianceQuadraticExponential)
     S0 :: Evolution.run(ceikit, t0, T, dt)
  }


  def launch() {
    Application.launch()
  }
}
