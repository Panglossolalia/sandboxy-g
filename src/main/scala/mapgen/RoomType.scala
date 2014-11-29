package mapgen

sealed abstract trait RoomType

case object Corridor extends RoomType
case object Barracks extends RoomType
case object GenericRoom extends RoomType
case object Workshop extends RoomType