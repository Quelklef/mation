module Mation where

import Mation.Prelude

import Effect.Ref (Ref)
import Effect.Ref as Ref


newtype Mation m s = Mation (ApplyStep s -> m Unit)
type ApplyStep s = forall n. MonadEffect n => (s -> s) -> n Unit

-- FIXME: I think @Lens'@ can be weakened to @Setter'@
embedMation :: forall m large small. Lens' large small -> Mation m small -> Mation m large
embedMation lens (Mation f) =
  Mation \apply -> f (\endo -> apply (lens %~ endo))

instance Apply m => Semigroup (Mation m s) where
  append (Mation f) (Mation g) = Mation (\step -> f step *> g step)

instance Applicative m => Monoid (Mation m s) where
  mempty = Mation (\_step -> pure unit)



foreign import data DOMNode :: Type

newtype Assoc k v = Assoc (Array (k /\ v))

instance Functor (Assoc k) where
  map f (Assoc arr) = Assoc $ map (\(a /\ b) -> a /\ f b) arr

toArray :: forall k v. Assoc k v -> Array (k /\ v)
toArray (Assoc arr) = arr

-- | Virtual DOM
data Html m s
  = HEmbed DOMNode
      -- ^ Include a given DOM node directly
  | HText String
      -- ^ Text node
  | HVirtual
      { tag :: String
      , attrs :: Assoc String String
      , listeners :: Assoc String (Mation m s)
          -- ^
          -- Knowledge of the listeners is needed here so that @embed@
          -- can be implemented
          --
          -- Otherwise we could put listeners in @fixup@
      , fixup :: DOMNode -> Effect Unit
      , children :: Array (Html m s)
      }

type Html' s = Html Effect s

txt :: forall m s. String -> Html m s
txt = HText

elt :: forall m s. String -> Array (Prop m s) -> Array (Html m s) -> Html m s
elt tag props children =
  HVirtual { tag, attrs, listeners, fixup, children }

  where

  attrs = Assoc $
    props # foldMap case _ of
      PPair k v -> [ k /\ v ]
      _ -> []

  listeners = Assoc $
    props # foldMap case _ of
      PListener k v -> [ k /\ v ]
      _ -> []

  fixup =
    props # foldMap case _ of
      PFixup f -> f
      _ -> mempty


data Prop m s
  = PPair String String
  | PListener String (Mation m s)
  | PFixup (DOMNode -> Effect Unit)

att :: forall m s. String -> String -> Prop m s
att = PPair

lis :: forall m s. String -> Mation m s -> Prop m s
lis = PListener






-- | Embed one @Html@ within another
embed :: forall m large small. Lens' large small -> Html m small -> Html m large
embed lens = case _ of
  HEmbed node -> HEmbed node
  HText text -> HText text
  HVirtual { tag, attrs, listeners, fixup, children } ->
    HVirtual { tag, attrs, fixup
             , listeners: map (embedMation lens) listeners
             , children: map (embed lens) children
             }

-- | Foreign interface Html type
newtype Html_f = Html_f (
    forall r.
       (DOMNode -> r)
    -> (String -> r)
    -> ({ tag :: String
        , attrs :: Array { name :: String, value :: String }
        , listeners :: Array { name :: String, handler :: Effect Unit }
        , fixup :: DOMNode -> Effect Unit
        , children :: Array Html_f
        } -> r)
    -> r
  )

to_f :: forall m s. (Mation m s -> Effect Unit) -> Html m s -> Html_f
to_f toEff html = Html_f \hembed htext hvirtual ->
  case html of
    HEmbed node -> hembed node
    HText text -> htext text
    HVirtual { tag, attrs, listeners, fixup, children } ->
      hvirtual
        { tag
        , attrs: attrs # toArray # map \(name /\ value) -> { name, value }
        , listeners: listeners # toArray # map \(name /\ mation) -> { name, handler: toEff mation }
        , fixup
        , children: to_f toEff <$> children
        }

foreign import renderHtml_f :: Html_f -> Effect DOMNode

renderHtml :: forall m s. (Mation m s -> Effect Unit) -> Html m s -> Effect DOMNode
renderHtml toEff = to_f toEff >>> renderHtml_f



-- FIXME: parameterize over DOMNode type, because I don't want to have to choose
--        a dom FFI library
runApp :: forall m s. MonadEffect m =>
  { initial :: s
  , render :: s -> Html m s
  , kickoff :: Mation m s
  , toEffect :: forall a. m a -> Effect a
  , root :: Effect DOMNode
  } -> Effect Unit

runApp args = do

  modelRef <- Ref.new args.initial

  let
    toEff :: Mation m s -> Effect Unit
    toEff (Mation f) =
      args.toEffect $
        f \endo -> liftEffect do
            Ref.modify_ endo modelRef
            doRender

    doRender :: Effect Unit
    doRender = do
      model <- Ref.read modelRef
      node <- renderHtml toEff (args.render model)
      root <- args.root
      mountUnder { container: root, mountMe: node }

  doRender



foreign import getBody :: Effect DOMNode

foreign import mountUnder :: { container :: DOMNode, mountMe :: DOMNode } -> Effect Unit
