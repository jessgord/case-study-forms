package forms

sealed trait Question[A] {
  def product[B](that: Question[B]): Question[(A, B)] =
    Product(this, that)
}
final case class Product[A, B](left: Question[A], right: Question[B])
    extends Question[(A, B)]

final case class IntQuestion(
    initialValue: Option[Int],
    label: Option[String],
    placeholder: Option[Int]
) extends Question[Int] {
  def withInitialValue(initialValue: Int): IntQuestion =
    this.copy(initialValue = Some(initialValue))

  def withoutInitialValue: IntQuestion =
    this.copy(initialValue = None)

  def withLabel(label: String): IntQuestion =
    this.copy(label = Some(label))

  def withPlaceholder(placeholder: Int): IntQuestion =
    this.copy(placeholder = Some(placeholder))
}

final case class TextQuestion(
    initialValue: Option[String],
    label: Option[String],
    placeholder: Option[String]
) extends Question[String] {
  def withInitialValue(initialValue: String): TextQuestion =
    this.copy(initialValue = Some(initialValue))

  def withoutInitialValue: TextQuestion =
    this.copy(initialValue = None)

  def withLabel(label: String): TextQuestion =
    this.copy(label = Some(label))

  def withPlaceholder(placeholder: String): TextQuestion =
    this.copy(placeholder = Some(placeholder))
}

object IntQuestion {
  def apply: IntQuestion =
    IntQuestion(None, None, None)
}

object TextQuestion {
  def apply: TextQuestion =
    TextQuestion(None, None, None)
}

// IntQuestion.apply.withInitialValue(0)

final case class BooleanQuestion(
    style: Style.BooleanStyle,
    initialValue: Option[Boolean],
    label: Option[String]
) extends Question[Boolean] {
  def withInitialValue(initialValue: Boolean): BooleanQuestion =
    this.copy(initialValue = Some(initialValue))

  def withLabel(label: String): BooleanQuestion =
    this.copy(label = Some(label))

  def withStyle(style: Style.BooleanStyle): BooleanQuestion =
    this.copy(style = style)
}
object BooleanQuestion {
  def apply: BooleanQuestion =
    BooleanQuestion(Style.boolean.checkbox, None, None)

  def empty: BooleanQuestion =
    BooleanQuestion.apply
}

// sealed trait IncomeLevel
// final case class Low() extends IncomeLevel
// final case class Mid() extends IncomeLevel
// final case class High() extends IncomeLevel
// Choose income range
//  0 - 10000 Low()
// 10000 - 20000 Mid()
// 20000+ High()
final case class MultipleChoice[A](label: Option[String], choices: List[(String, A)])
    extends Question[List[A]] {
  def withLabel(label: String): MultipleChoice[A] =
    this.copy(label = Some(label))
}
object MultipleChoice {
  def apply[A](choices: List[(String, A)]): MultipleChoice[A] =
    MultipleChoice(None, choices)
}
