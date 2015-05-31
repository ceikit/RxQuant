package main
import rx.lang.scala.Observable

import scala.collection.mutable.Queue



object Extensions {


  implicit class TechnicalIndicator(series: Observable[Double]) {

    def arithmeticMA(n: Int): Observable[Double] = {

      var movAvg: Double = 0
      var value: Queue[Double] = Queue()
      var i = 0

      Observable[Double](subscriber => {

        series.subscribe(v => {
          i = i + 1
          if (i < n) {
            movAvg = movAvg + v
            value.enqueue(v)
            subscriber.onNext(v) }
          else {
            if (i == n ) {
              movAvg = movAvg + v
              movAvg = movAvg / n
              value.enqueue(v)  // current value of series updated
              value.dequeue()  // removing the head of the queue
              subscriber.onNext(movAvg)
            }
          else {
            if (i > n) {
              movAvg = movAvg  + (v - value.dequeue) / n // MA computation
              value.enqueue(v) // current value of series updated
              subscriber.onNext(movAvg) // sending value to subscriber
            }
          }
        }
        })
      })
    }




    def arithmeticMA_Functional (bufferSize: Int): Observable[Double] = {
      series
        .slidingBuffer(bufferSize, 1)
        .scan(-1.0)( (prevMovAvg, b) => prevMovAvg match {
        case -1 => b.foldRight(0.0){ _ + _ }/ bufferSize
        case _  => prevMovAvg + (b.last - b.head) / bufferSize
        } )
        .drop(1) // because scan return also the seed (-1)
    }




      def exponentialMA(alpha: Double): Observable[Double] = {
        var movAvg: Double = 0
        var i = 0
        Observable[Double](subscriber => {
          series.subscribe(v => {
            i = i + 1
            if (i == 1)  { movAvg = v; subscriber.onNext(v) }
            else { movAvg = (1 - alpha) * v + alpha * movAvg; subscriber.onNext(movAvg)}
          })
        })
      }



      def exponentialMA_Functional (alpha: Double): Observable[Double] = {
        series . scan ( (movAvg, v)  => (1 - alpha) * v + alpha * movAvg )
      }



      def exponentialMovingVolatility(alpha: Double, series : Observable[Double]) : Observable[Double] = {

        var movAvg: Double = 0
        var movVar: Double = 0.044
        var i = 0

        Observable[Double]( subscriber => {
          series.subscribe( v => {
            i = i+1
            if( i == 1) {movAvg = v ; subscriber.onNext(movVar) }
            else {
              movAvg = (1 - alpha)* v + alpha * movAvg
              movVar = (1 - alpha)*math.pow(v - movAvg,2) + alpha*movVar
              subscriber.onNext(math.sqrt(movVar)) // return the sqrt of Var which is the Volatility
            }

          })
        })

      }





  }

}