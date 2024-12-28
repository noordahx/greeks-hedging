package com.example.domain

sealed trait Position {
    def ticker: String
    def quantity: Int
    def delta: Double
}

final case class StockPosition(
    ticker: String,
    quantity: Int,
    stockPrice: Double,
) extends Position {
    override def delta: Double = 1.0 * quantity
}

final case class OptionPosition(
    ticker: String,
    quantity: Int,
    strike: Double,
    expiry: String,
    impliedVol: Double,
    underlyingPrice: Double,
    interestRate: Double,
    isCall: Boolean
) extends Position {
    override val delta: Double = {
        // Black-Scholes
        // quantity * [Black-Scholes call/put delta]
        val d = BlackScholes.delta(
            underlyingPrice, strike, timeToMaturity(expiry), interestRate, impliedVol, isCall
        )
        d * quantity
    }

    private def timeToMaturity(exp: String): Double = {
        val now = java.time.LocalDate.now
        val expiry = java.time.LocalDate.parse(exp) // format: "yyyy-MM-dd"
        java.time.temporal.ChronoUnit.DAYS.between(now, expiry) / 365.0
    }
}

