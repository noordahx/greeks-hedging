package com.example

import com.example.domain._
import com.example.services.YahooService
import sttp.client3._

import scala.io.StdIn

object Main extends App {
    println("Example Portfolio")

    val aaplStockPosition: StockPosition = StockPosition(
        ticker = "AAPL",
        quantity = 100,
        stockPrice = 0,
    )

    val tslaOptionPosition: OptionPosition = OptionPosition(
        ticker = "TSLA",
        quantity = 2,
        strike = 200.0,
        expiry = "2024-12-31",
        impliedVol = 0.5,
        underlyingPrice = 0,
        interestRate = 0.05,
        isCall = true
    )

    val portfolio = Portfolio(List(aaplStockPosition, tslaOptionPosition))

    println("Fetching data from Yahoo for each position")
    
    portfolio.positions.foreach {
        case sp: StockPosition =>
            YahooService.fetchStockPrice(sp.ticker) match {
                case Right(price) =>
                    println(s"Fetched price for ${sp.ticker}: $price")
                    // Stock delta = 1 * qty
                case Left(err) =>
                    println(s"Error fetching price for ${sp.ticker}: $err")
            }
        case op: OptionPosition =>
            val underlyingTicker = op.ticker
            val underlyingPriceE = YahooService.fetchStockPrice(underlyingTicker)
            underlyingPriceE match {
                case Right(spotPrice) =>
                    println(s"Fetched spot price for ${op.ticker}: $spotPrice")
                case Left(err) =>
                    println(s"Error fetching spot price for ${op.ticker}: $err")
            }

            val chainE = YahooService.fetchOptionChain(underlyingTicker)
            chainE match {
                case Right(optList) =>
                    val maybeThisStrike = optList.find(_.strike == op.strike)
                    maybeThisStrike match {
                        case Some(optData) =>
                            val iv = optData.impliedVolatility.getOrElse(0.4)
                            println(s"Found option with strick ${op.strike} and IV $iv")
                        case None =>
                            println(s"Could not find option with strike ${op.strike}")
                    }
                case Left(err) =>
                    println(s"Error fetching option chain for ${op.ticker}: $err")
            }

        case _ =>
            println("Unknown position type")

    }

    val netDelta = portfolio.totalDelta
    println(s"Net delta: $netDelta")

    // println("Enter the ticker you want to use as the hedging option (e.g., 'SPY'):")
    // val hedgeTicker = StdIn.readLine()

    // val chainE = YahooService.fetchOptionChain(hedgeTicker)
    // chainE match {

    // }
}