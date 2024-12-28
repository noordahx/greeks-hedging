package com.example.domain

import math._

object BlackScholes {
    /**
        S = spot
        K = strike
        t = time to maturity
        r = risk-free rate
        vol = implied volatility
        isCall = true if call, false if put
    **/
    def delta(S: Double, K: Double, t: Double, r: Double, vol: Double, isCall: Boolean): Double = {
        if (t <= 0.0) {
            // If expired or t = 0, call delta ~1 if S > K else 0, similar for put
            if (isCall) {
                if (S > K) 1.0 else 0.0
            } else {
                if (S > K) 0.0 else -1.0
            }
        } else {
            val d1 = (log(S / K) + (r + vol / 2.0) * t) / (vol * sqrt(t))
            val nd1 = cdf(d1)
            if (isCall) nd1 else nd1 - 1.0
        }
    }

    /**
        Standard normal cumulative distribution function
    **/
    private def cdf(x: Double): Double = 0.5 * (1.0 + erf(x / sqrt(2.0)))

    /**
        Gauss error function
    **/
    private def erf(z: Double): Double = {
        val t = 1.0 / (1.0 + 0.5 * abs(z))
        val ans = 1 - t * exp( -z*z - 1.26551223 +
            t * ( 1.00002368 +
            t * ( 0.37409196 +
            t * ( 0.09678418 +
            t * (-0.18628806 +
            t * ( 0.27886807 +
            t * (-1.13520398 +
            t * ( 1.48851587 +
            t * (-0.82215223 +
            t * ( 0.17087277))))))))))
        if (z >= 0) ans else -ans
    }
}