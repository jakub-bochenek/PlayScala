package controllers

import play.api.libs.json.Json
import play.api.data.Form
import play.api.data.Forms.{mapping, number, text}
import play.api.mvc.{AbstractController, AnyContent, ControllerComponents, Request}
import play.filters.csrf._
import javax.inject.Inject
import play.api.libs.json.{Json, JsValue}

case class Product(id: Int, name: String, price: Int)

object Product {
  val form: Form[Product] = Form(
    mapping(
      "id" -> number,
      "name" -> text,
      "price" -> number
    )(Product.apply)(Product.unapply)
  )
}


class ProductController @Inject()(cc: ControllerComponents) extends AbstractController(cc) with play.api.i18n.I18nSupport {

  var listProducts = Seq(
    Product(1, "Produkt 1", 10),
    Product(2, "Produkt 2", 15),
  )

  def update(id: Int, newAge: Int) = Action { implicit request =>
    listProducts.find(_.id == id) match {
      case Some(form) =>
        val updatedForm = form.copy(price = newAge)
        listProducts = listProducts.filterNot(_.id == id) :+ updatedForm // replace the old form with the updated one
        Ok("Modification success")
      case None => NotFound(s"Form with id $id not found") // return a 404 error if the form doesn't exist
    }
  }

  def simpleForm() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.basicForm(Product.form))
  }


  def simpleFormPost() = Action { implicit request =>
    val formData: Product = Product.form.bindFromRequest.get
    listProducts :+= formData

    val formJson: JsValue = Json.obj(
      "id" -> formData.id,
      "name" -> formData.name,
      "age" -> formData.price
    )
    Ok(formJson.toString())
  }

  def index() = Action { implicit request =>

    Ok(views.html.index(listProducts))
  }

  def delete(id: Long) = Action {
    listProducts = listProducts.filter(_.id != id)
    Ok(s"Usunięto element o id: $id")
  }


  def getById(id: Int) = Action { implicit request =>
    listProducts.find(_.id == id) match {
      case Some(form) => {
        val formJson: JsValue = Json.obj(
          "id" -> form.id,
          "name" -> form.name,
          "age" -> form.price
        )
        Ok(formJson.toString())
      }
      case None => {
        val errorJson: JsValue = Json.obj("error" -> s"Formularz o id $id nie został znaleziony.")
        NotFound(errorJson.toString())
      }
    }
  }

}