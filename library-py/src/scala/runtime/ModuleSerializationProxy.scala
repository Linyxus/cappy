package scala.runtime

/** Marker class for module-singleton serialization.
 *
 *  Inert in the Python backend — modules are not serialized.
 */
class ModuleSerializationProxy(cls: java.lang.Class[?])
