import scala.io.StdIn
import sttp.client3._
import sttp.client3.circe._
import io.circe._
import io.circe.parser._
import math._
import scala.util.Try
import io.circe.generic.semiauto._

case class OptionPosition(
  underlying: String,
  optionType: String, // call/put
  strike: Double,
  maturity: Double,  // T = x yrs
  quantity: Int
)

case class StockPosition(
  symbol: String,
  quantity: Int
)

case class Portfolio(
  stocks: List[StockPosition],
  options: List[OptionPosition]
)

case class MarketData(
  spot: Double,
  dividentYield: Double,
  interestRate: Double,
  volatility: Double
)


// Supports Delta heding for now

case class Greeks(
  delta: Double,
)

object BlackScholes {
  def CDN(x: Double): Double = {
    val a1 = 0.31938150
    val a2 = -0.356563782
    val a3 = 1.781477937
    val a4 = -1.821255978
    val a5 = 1.330274429
    val L = math.abs(x)
    val k = 1.0/(1.0+0.2316419*L)
    val w = 1.0 - 1.0/sqrt(2.0 * Pi)*exp(-L*L/2.0)*(a1*k+a2*k*k+a3*k*k*k+a4*k*k*k*k+a5*k*k*k*k*k)
    if (x<0.0) 1.0-w else w
  }

  def d1(spot: Double, strike: Double, r: Double, q: Double, vol: Double, T: Double): Double = {
    (math.log(spot/strike) + (r - q + 0.5*vol*vol)*T) / (vol*math.sqrt(T))
  }

  def delta(spot: Double, strike: Double, r: Double, q: Double, vol: Double, T: Double, optType: String): Double = {
    val D1 = d1(spot, strike, r, q, vol, T)
    val Nd1 = CDN(D1)
    optType.toLowerCase match {
      case "call" => math.exp(-q*T)*Nd1
      case "put" => math.exp(-q*T)*(Nd1 - 1.0)
    }

  }
}

object YahooFinance {
  case class QuoteItem(
    symbol: String,
    regularMarketPrice: Double
  )
  case class QuoteResponse(result: List[QuoteItem])
  case class YahooResponse(quoteResponse: QuoteResponse)

  implicit val quoteItemDecoder: Decoder[QuoteItem] = deriveDecoder[QuoteItem]
  implicit val quoteResponseDecoder: Decoder[QuoteResponse] = deriveDecoder[QuoteResponse]
  implicit val yahooResponseDecoder: Decoder[YahooResponse] = deriveDecoder[YahooResponse]


  def getMarketData(symbol: String): Option[MarketData] = {
    val backend = HttpURLConnectionBackend()
    val url = uri"http://query1.finance.yahoo.com/v7/finance/quote?symbols=$symbol"
    val response = basicRequest.get(url).response(asJson[YahooResponse]).send(backend)

    response.body.toOption.flatMap { yresp =>
      yresp.quoteResponse.result.headOption.map { quoteItem =>
        // Fix r, D, sigma for demo
        // TODO: get those data from API
        MarketData(
          spot = quoteItem.regularMarketPrice,
          dividentYield = 0.005, // 0.5%
          interestRate = 0.05, // 5% in Hong Kong
          volatility = 0.20    // assume 20%
        )
      }
    }
  }
}

object PortfolioDelta {
  def compute(portfolio: Portfolio): Double = {
    val optionDeltas = portfolio.options.flatMap { opt =>
      YahooFinance.getMarketData(opt.underlying).map { md =>
        val d = BlackScholes.delta(
          spot = md.spot,
          strike = opt.strike,
          r = md.interestRate,
          q = md.dividentYield,
          vol = md.volatility,
          T = opt.maturity,
          optType = opt.optionType
        )
        // each option contract * 100
        d * opt.quantity * 100
      }
    }.sum

    val stockDeltas = portfolio.stocks.map(_.quantity.toDouble).sum
  
    optionDeltas + stockDeltas
  }
}

object DeltaHedger {
  // to hedge delta D, trade -D shares of underlying
  def hedgeDelta(portfolioDelta: Double): Double = -portfolioDelta
}

@main def runDeltaHedging(): Unit = {
  println("Enter the stock symbol of underlying (e.g. AAPL):")
  val stockSymbol = StdIn.readLine().trim.toUpperCase

  println("Enter the number of shares:")
  val stockQty = Try(StdIn.readLine().toInt).getOrElse(0)

  println("Do you have any options on this stock? (Y/n)")
  val hasOptions = StdIn.readLine().trim.toLowerCase.startsWith("y")

  var optionsList: List[OptionPosition] = Nil

  if (hasOptions) {
    println("How many options do you have?")
    val optCount = Try(StdIn.readLine().toInt).getOrElse(0)
    for (_ <- 1 to optCount) {
      println("Enter option details as: optionType(call/put), strike, maturity(in years), quantity")
      println("Eaxmple: call,150.0,0.5,10")
      val line = StdIn.readLine().split(",").map(_.trim)
      if (line.length == 4) {
        val optType = line(0)
        val strike = Try(line(1).toDouble).getOrElse(0.0)
        val maturity = Try(line(2).toDouble).getOrElse(0.5)
        val quantity = Try(line(3).toInt).getOrElse(0)
        optionsList = OptionPosition(stockSymbol, optType, strike, maturity, quantity) :: optionsList
      } else {
        println("Invalid input, skipping this option")
      }
    }
  }

  val portfolio = Portfolio(
    stocks = List(StockPosition(stockSymbol, stockQty)),
    options = optionsList,
  )

  // Compute portfolio delta
  val pDelta = PortfolioDelta.compute(portfolio)
  println(s"Current Portfolio Delta: $pDelta")

  // Ask if user wants to hedge delta
  println("Do you want to hedge the delta by trading the underlying stock? (y/n)")
  val hedgeChoice = StdIn.readLine().trim.toLowerCase
  if (hedgeChoice == "y") {
    val hedgeShares = DeltaHedger.hedgeDelta(pDelta)
    if (hedgeShares > 0) {
      println(s"To hedge your delta, you can BUY approx ${hedgeShares.toInt} shares of $stockSymbol.")
    } else if (hedgeShares < 0) {
      println(s" To hedge your delta, you can SELL approx ${-hedgeShares.toInt} shares of $stockSymbol.")
    } else {
      println("Your delta is already near zero, no hedge required.")
    }
  } else {
    println("No hedging performed")
  }

}


