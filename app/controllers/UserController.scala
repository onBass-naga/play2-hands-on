package controllers

import play.api.mvc._
import play.api.db.slick._
import play.api.data._
import play.api.data.Forms._
import models.Tables._
import profile.simple._


/**
 * Created by naga on 15/04/14.
 */
object UserController extends Controller {

  // フォームの値を格納する
  case class UserForm(id: Option[Long], name: String, companyId: Option[Int])

  // formのデータ⇔ケースクラスの変換を行う
  val userForm = Form(
    mapping(
      "id"        -> optional(longNumber),
      "name"      -> nonEmptyText(maxLength = 20),
      "companyId" -> optional(number)
    )(UserForm.apply)(UserForm.unapply)
  )

  /**
   * 一覧表示
   */
  def list = DBAction { implicit rs =>
    // ユーザをIDの昇順でソートして取得
    val users = Users.sortBy(t => t.id).list

    // テンプレートをレンダリングしてレスポンスを返却
    Ok(views.html.user.list(users))
  }

  /**
   * 登録・編集画面表示
   */
  def edit(id: Option[Long]) = DBAction { implicit rs =>
    // リクエストパラメータにIDが存在する場合
    val form = if(id.isDefined) {
      // IDからユーザ情報を1件取得
      val user = Users.filter(_.id === id.get.bind).first

      // 値をフォームに詰める
      userForm.fill(UserForm(Some(user.id), user.name, user.companyId))
    } else {
      // リクエストパラメータにIDが存在しない場合
      userForm
    }

    // 会社一覧を取得
    val companies = Companies.sortBy(t => t.id).list

    Ok(views.html.user.edit(form, companies))
  }

  /**
   * 登録実行
   */
  def create = DBAction.transaction { implicit rs =>
    // リクエストの内容をバインド
    userForm.bindFromRequest.fold(
      // エラーの場合は登録画面に戻す
      error => BadRequest(views.html.user.edit(error, Companies.sortBy(t => t.id).list)),

      // OKの場合は登録を行い一覧画面にリダイレクトする
      form  => {
        // ユーザを登録
        val user = UsersRow(0, form.name, form.companyId)
        Users.insert(user)

        // 一覧画面にリダイレクト
        Redirect(routes.UserController.list)
      }
    )
  }

  /**
   * 更新実行
   */
  def update = DBAction.transaction { implicit rs =>
    // リクエストの内容をバインド
    userForm.bindFromRequest.fold(
      // エラーの場合
      error => BadRequest(views.html.user.edit(error, Companies.sortBy(t => t.id).list)),

      // OKの場合
      form  => {
        // ユーザ情報を更新
        val user = UsersRow(form.id.get, form.name, form.companyId)
        Users.filter(_.id === user.id.bind).update(user)

        // 一覧画面へリダイレクト
        Redirect(routes.UserController.list)
      }
    )
  }

  /**
   * 削除実行
   */
  def remove(id: Long) = DBAction.transaction { implicit rs =>
    // ユーザIDを指定して削除
    Users.filter(t => t.id === id.bind).delete

    // 一覧画面へリダイレクト
    Redirect(routes.UserController.list)
  }

}