package main

import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.chart.{LineChart, NumberAxis, XYChart}
import javafx.stage.Stage

import main.Extensions.TechnicalIndicator
import main.types._
import rx.lang.scala.Observable

import scala.concurrent.duration._
import scala.util.Success


class LineChartReactiveStochasticProcess extends Application {


  override def start(stage: Stage): Unit = {

    stage.setTitle("Heston model")

    //defining the axes
    val xAxis = new NumberAxis()
    val yAxis = new NumberAxis()
    xAxis.setLabel("Ticks")

    //creating the chart
    val lineChart = new LineChart[Number, Number](xAxis, yAxis)

    lineChart.setTitle("QuantCeikit")
    //defining a series

    val series1 = new XYChart.Series[Number, Number]()
    series1.setName("Volatility")

    val series2 = new XYChart.Series[Number, Number]()
    series2.setName("Stock")

    val series3 = new XYChart.Series[Number, Number]()
    series3.setName("Moving Average")

    val series4 = new XYChart.Series[Number, Number]()
    series4.setName("Moving Average")

    val series5 = new XYChart.Series[Number, Number]()
    series5.setName("Moving Volatility")

    val scene = new Scene(lineChart, 1200, 800)


    stage.setScene(scene)
    stage.show()

    readLine()

    val result = heston().publish

    val index = Observable.interval(50 milliseconds).onBackpressureBuffer

    val n1 : Int = 10
    val n2 : Int = 20


    val movingAverage1 = result.map(v => v._2).arithmeticMA(n1)
    val movingAverage2 = result.map(v => v._2).arithmeticMA_Functional(n2)

    val movingAverage3 = result.map(v => v._2).exponentialMA(0.90)
    val movingAverage4 = result.map(v => v._2).exponentialMA_Functional(0.98)

    val chart = Observable.zip(index, result, movingAverage3, movingAverage4)

    chart.observeOn(FXScheduler()).subscribe(v =>{

      series3.getData.add(new XYChart.Data(v._1 , v._3 ) )
      series4.getData.add(new XYChart.Data(v._1 , v._4 ) )


      series1.getData.add(new XYChart.Data(v._1 , math.sqrt(v._2._1)*10 ) )
      series2.getData.add(new XYChart.Data(v._1 , v._2._2 ))


    } )

    result.connect /// THIS IS THE SHIT

    lineChart.getData.addAll(series1, series2, series3, series4)
  }

  def reactiveResults(): Observable[Double] = {
    val t0: Time = 0 // initial time
    val S0: Double = 10 // initial value of the stock
    val N = 1000 // number of time-steps
    val T: Time = 5 // maturity
    val dt = (T - t0) / N // time-step length


    val ceikit = new GeometricBrownian(0.05, 0.5, Success(S0), Discretization.euler)
    //val ceikit = new CIR(10, 0.04, 0.5, Success(S0), Discretization.varianceQuadraticExponential)

    println("start")

    S0 +: ReactiveEvolution.reactiveRun(ceikit, t0, T, dt)

  }



  def heston(): Observable[(Double,Double)] = {

    val t0: Time = 0 // initial time
    val N = 1000 // number of time-steps
    val T: Time = 5 // maturity
    val dt = (T - t0) / N // time-step length
    val S0: Double = 10 // initial value of the stock
    val v0: (Double, Double) = (0.044 ,0.044)
    val ceikit = new Heston(0.03, - 0.6288, 14.829, 0.04, 0.7, v0, Success(S0), Discretization.stockQE)

    (v0._2 , S0) +: EvolutionHeston.runHeston(ceikit, t0, T, dt)

  }

  // (k 1.4829, thetha 0.0872, nu 1.7048, rho −0.6288, v 0.0104)
  // 0.03, -0.7, 20, 0.04, 0.9



  def launch() {
    Application.launch()
  }
}
