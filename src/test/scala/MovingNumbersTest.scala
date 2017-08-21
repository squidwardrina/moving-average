import java.security.InvalidParameterException

import org.scalatest.{Matchers, WordSpec}

class MovingNumbersTest extends WordSpec with Matchers {
  "Getting moving average" should {

    "Produce exception when getting average before ingesting" in {
      val movingNumbers = new MovingNumbers
      intercept[NoSuchElementException](movingNumbers.getMovingAVG(1))
    }

    "Produce exception when passing 0 or negative argument" in {
      val movingNumbers = new MovingNumbers
      movingNumbers.ingest(1)
      intercept[InvalidParameterException](movingNumbers.getMovingAVG(0))
      intercept[InvalidParameterException](movingNumbers.getMovingAVG(-1))
    }

    "Compute average on 1 number" in {
      val movingNumbers = new MovingNumbers
      movingNumbers.ingest(5)
      movingNumbers.getMovingAVG(1) shouldEqual 5
    }

    "Compute average on 2 numbers" in {
      val movingNumbers = new MovingNumbers
      movingNumbers.ingest(1)
      movingNumbers.ingest(2)
      movingNumbers.getMovingAVG(2) shouldEqual 1.5
    }

    "Compute average of 2 last numbers out of 3" in {
      val movingNumbers = new MovingNumbers
      movingNumbers.ingest(5)
      movingNumbers.ingest(1)
      movingNumbers.ingest(2)
      movingNumbers.getMovingAVG(2) shouldEqual 1.5
    }

    "Compute average of 4 numbers" in {
      val movingNumbers = new MovingNumbers
      movingNumbers.ingest(2)
      movingNumbers.ingest(6)
      movingNumbers.ingest(8)
      movingNumbers.ingest(12)
      movingNumbers.getMovingAVG(4) shouldEqual 7
    }

    "Count average of all values when 'seconds' is bigger than values amount" in {
      val movingNumbers = new MovingNumbers
      movingNumbers.ingest(1)
      movingNumbers.ingest(2)
      movingNumbers.getMovingAVG(5) shouldEqual 1.5
    }

    "Produce different result after ingesting new number" in {
      val movingNumbers = new MovingNumbers
      movingNumbers.ingest(1)
      movingNumbers.ingest(2)
      val firstCall = movingNumbers.getMovingAVG(2)
      movingNumbers.ingest(3)
      val secondCall = movingNumbers.getMovingAVG(2)
      (firstCall, secondCall) shouldEqual(1.5, 2.5)
    }
  }
}
