case class UtilityFunctions () {

  // Not currently used
  def iterateToList(obj: AnyRef): List[Any] = {
    val fields = obj.getClass.getDeclaredFields
    fields.filter(_.getName != "MODULE$").map {
      field =>
        field.setAccessible(true)
        field.get(obj)
    }.toList
  }

}
