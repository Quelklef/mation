
-- | Since this framework is in the business of dealing with the DOM, it
-- | has to handle DOM-related values, like nodes and events.
-- | 
-- | Instead of committing to any particular Purescript DOM API, we stay
-- | ambivalent. All we need is a type for DOM Nodes and a type for DOM Events.
-- | However, the user may choose what types those are; if they implement
-- | the below typeclasses then this framework will happily accept them.

-- FIXME: we don't actually *use* IsDom{Node,Event} anywhere tho

module Mation.Core.Dom where
  
import Mation.Core.Prelude
import Mation.Core.Util.UnsureEq (class UnsureEq, viaPrim)


-- | Signifies that a type should be used as a DOM Node type
-- |
-- | Instantiate this to use a custom DOM Node type
class Coercible node Foreign <= IsDomNode node

-- | Signifies that a type should be used as a DOM Event type
-- |
-- | Instantiate this to use a custom DOM Node type
class Coercible event Foreign <= IsDomEvent event


-- | Built-in concrete DOM Node type (this is just a newtype over Foreign)
-- |
-- | May be `coerce`d to any instance of `IsDomeNode`
newtype DomNode = DomNode Foreign

derive instance Newtype DomNode _
instance IsDomNode DomNode

-- | Instance given by `viaPrim`
instance UnsureEq DomNode where
  unsureEq = viaPrim

-- | Built-in concrete DOM Event type (this is just a newtype over Foreign)
-- |
-- | May be `coerce`d to any instance of `IsDomEvent`
newtype DomEvent = DomEvent Foreign

derive instance Newtype DomEvent _
instance IsDomEvent DomEvent

-- | Instance given by `viaPrim`
instance UnsureEq DomEvent where
  unsureEq = viaPrim

