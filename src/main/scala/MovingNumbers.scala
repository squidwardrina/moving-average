import java.security.InvalidParameterException

class MovingNumbers {
  // Using a linked list because prepending is O(1)
  // The list is immutable, so it is thread-safe. When adding a value new immutable list is created.
  private var values = List.empty[Int]

  /**
    * Complexity:
    *
    * Time: O(1) - prepending to linked list.
    * Space: O(1) - each new list is created by prepending new head to existing list.
    */
  def ingest(value: Int) {
    values = value :: values
  }

  /**
    * Complexity:
    *
    * Time - O(n):
    * --------------------
    * | take | foldLeft  |
    * --------------------
    * | O(n) |    O(n)   |
    * --------------------
    *
    * Space - O(n):
    * --------------------------------------
    * | seconds | relevantVals | foldLeft  |
    * --------------------------------------
    * |   O(1)  |     O(n)     |    O(1)   |
    * --------------------------------------
    */
  def getMovingAVG(seconds: Int): Double = {
    // Validate
    if (seconds <= 0) throw new InvalidParameterException("Number of seconds should be positive.")
    if (values.isEmpty) throw new NoSuchElementException("No values ingested yet.")

    // Take needed number of last values (if there are less - take all)
    val relevantVals = values.take(seconds)

    // Efficiently calculating average avoiding recalculating list size (list size needs recalculation as it can be smaller than 'seconds')
    relevantVals.foldLeft((0.0, 1)) { case ((prevAvg, count), currVal) =>
      (prevAvg + ((currVal - prevAvg) / count), count + 1)
    }._1

    // ---- Easier but less efficient average calculation: ----
    //    relevantVals.sum / relevantVals.size.toDouble
  }
}
