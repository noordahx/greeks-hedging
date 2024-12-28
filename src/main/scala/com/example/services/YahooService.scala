package com.example.services

import sttp.client3._
import sttp.client3.circe._
import io.circe._
import io.circe.parser._
import io.circe.generic.auto._

object YahooService {
    case class QuoteResult(
        symbol: String,
        regularMarketPrice: Option[Double],
        regularMarketChange: Option[Double],
        regularMarketVolume: Option[Long]
    )
    case class QuoteResponse(result: List[QuoteResult])
    case class QuoteResponseContainer(quoteResponse: QuoteResponse)

    // Option chain
    case class OptionData(
        strike: Double,
        expiration: Long,
        impliedVolatility: Option[Double],
        lastPrice: Option[Double],
    )

    case class Options(calls: List[OptionData], puts: List[OptionData])
    case class OptionChainResult(
        options: List[Options]
    )
    case class OptionChainResponse(
        result: List[OptionChainResult]
    )
    case class OptionChainContainer(
        optionChain: OptionChainResponse
    )

    def fetchStockPrice(ticker: String): Either[String, Double] = {
        val backend = HttpURLConnectionBackend()
        val resquest = basicRequest
            .get(uri"https://query1.finance.yahoo.com/v7/finance/quote?symbols=${ticker}")
            .response(asJson[QuoteResponseContainer])
        
        val response = resquest.send(backend)
        backend.close()

        response.body.left.map(_.toString).flatMap { container => 
            container.quoteResponse.result.headOption.flatMap(_.regularMarketPrice) match {
                case Some(price) => Right(price)
                case None => Left(s"No price found for $ticker")
            }
        }
    }

    def fetchOptionChain(ticker: String): Either[String, List[OptionData]] = {
        val backend = HttpURLConnectionBackend()
        val resquest = basicRequest
            .get(uri"https://query1.finance.yahoo.com/v7/finance/options/${ticker}")
            .response(asJson[OptionChainContainer])
        
        val response = resquest.send(backend)
        backend.close()

        response.body.left.map(_.toString).flatMap { container => 
            for {
                chainResult <- container.optionChain.result.headOption.toRight(s"No option chain found for $ticker")
                chian       <- chainResult.options.headOption.toRight(s"No options array found for $ticker")
            } yield chian.calls ++ chian.puts
        }
    }
}