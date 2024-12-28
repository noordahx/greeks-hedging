package com.example.domain
final case class Portfolio(positions: List[Position]) {
    def totalDelta: Double = positions.map(_.delta).sum

    def addPosition(pos: Position): Portfolio = copy(positions = positions :+ pos)
}