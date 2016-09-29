package org.matthicks.media4s.video

case class AudioInfo(codec: String = null, range: Long = 0L, stereo: Boolean = false, meta: MetaData = MetaData()) {
  override def toString = {
    s"AudioInfo(codec: $codec, range: $range, stereo: $stereo, meta: $meta)"
  }
}
