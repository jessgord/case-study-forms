package forms

import com.raquo.laminar.api.L._
import org.scalajs.dom

object Main {
  val appContainer: dom.Element = dom.document.querySelector("#appContainer")
  val weather = TextQuestion.apply.withLabel("What is the weather like?")
  val awesome = BooleanQuestion.apply.withLabel("Is everything awesome?")
  val cats = BooleanQuestion.apply.withStyle(Style.boolean.choice("Cats", "Moar Cats")).withLabel("Cats?")
  val multiChoice = MultipleChoice(List(("42", 42), ("Zero", 0), ("One", 1))).withLabel("What is the meaning of life")
  val numbers = IntQuestion.apply.withLabel("What's your favourite number?")
  val question = weather.product(awesome).product(cats).product(numbers).product(multiChoice)

  val form = Form("The Awesome Form", question)
   
  val appElement: HtmlElement =
    Render.render(form)

  def main(args: Array[String]): Unit =
    renderOnDomContentLoaded(appContainer, appElement)
}
