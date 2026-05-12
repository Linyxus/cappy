// v1 scope: an `@extern` Scala declaration is allowed as the single
// primary `extends` parent of a Scala class, but NOT as a mix-in
// trait/interface. `GenPython.genBases` rejects extern annotations in
// the interfaces slot with a clear error pointing at the subclass
// whose interface list contains it.

import scala.python.*

@extern("mod_inherit", "Mod") trait ModTrait
@extern("mod_inherit", "Mod") trait OtherTrait

class HasOneMix extends Object, ModTrait // error
class HasTwoMix extends Object, ModTrait, OtherTrait // error
