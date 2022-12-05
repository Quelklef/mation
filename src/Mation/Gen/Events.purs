module Mation.Gen.Events where

import Mation.Core.Mation (Mation)
import Mation.Core.Html (DOMEvent, Prop (..))


onAbort :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onAbort = PListener "abort"

onAfterprint :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onAfterprint = PListener "afterprint"

onAnimationend :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onAnimationend = PListener "animationend"

onAnimationiteration :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onAnimationiteration = PListener "animationiteration"

onAnimationstart :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onAnimationstart = PListener "animationstart"

onBeforeprint :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onBeforeprint = PListener "beforeprint"

onBeforeunload :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onBeforeunload = PListener "beforeunload"

onBlur :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onBlur = PListener "blur"

onCanplay :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onCanplay = PListener "canplay"

onCanplaythrough :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onCanplaythrough = PListener "canplaythrough"

onChange :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onChange = PListener "change"

onClick :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onClick = PListener "click"

onContextmenu :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onContextmenu = PListener "contextmenu"

onCopy :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onCopy = PListener "copy"

onCut :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onCut = PListener "cut"

onDblclick :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onDblclick = PListener "dblclick"

onDrag :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onDrag = PListener "drag"

onDragend :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onDragend = PListener "dragend"

onDragenter :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onDragenter = PListener "dragenter"

onDragleave :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onDragleave = PListener "dragleave"

onDragover :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onDragover = PListener "dragover"

onDragstart :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onDragstart = PListener "dragstart"

onDrop :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onDrop = PListener "drop"

onDurationchange :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onDurationchange = PListener "durationchange"

onEnded :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onEnded = PListener "ended"

onError :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onError = PListener "error"

onFocus :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onFocus = PListener "focus"

onFocusin :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onFocusin = PListener "focusin"

onFocusout :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onFocusout = PListener "focusout"

onFullscreenchange :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onFullscreenchange = PListener "fullscreenchange"

onFullscreenerror :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onFullscreenerror = PListener "fullscreenerror"

onHashchange :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onHashchange = PListener "hashchange"

onInput :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onInput = PListener "input"

onInvalid :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onInvalid = PListener "invalid"

onKeydown :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onKeydown = PListener "keydown"

onKeypress :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onKeypress = PListener "keypress"

onKeyup :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onKeyup = PListener "keyup"

onLoad :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onLoad = PListener "load"

onLoadeddata :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onLoadeddata = PListener "loadeddata"

onLoadedmetadata :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onLoadedmetadata = PListener "loadedmetadata"

onLoadstart :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onLoadstart = PListener "loadstart"

onMessage :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onMessage = PListener "message"

onMousedown :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onMousedown = PListener "mousedown"

onMouseenter :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onMouseenter = PListener "mouseenter"

onMouseleave :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onMouseleave = PListener "mouseleave"

onMousemove :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onMousemove = PListener "mousemove"

onMouseover :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onMouseover = PListener "mouseover"

onMouseout :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onMouseout = PListener "mouseout"

onMouseup :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onMouseup = PListener "mouseup"

onMousewheel :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onMousewheel = PListener "mousewheel"

onOffline :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onOffline = PListener "offline"

onOnline :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onOnline = PListener "online"

onOpen :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onOpen = PListener "open"

onPagehide :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onPagehide = PListener "pagehide"

onPageshow :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onPageshow = PListener "pageshow"

onPaste :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onPaste = PListener "paste"

onPause :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onPause = PListener "pause"

onPlay :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onPlay = PListener "play"

onPlaying :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onPlaying = PListener "playing"

onPopstate :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onPopstate = PListener "popstate"

onProgress :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onProgress = PListener "progress"

onRatechange :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onRatechange = PListener "ratechange"

onResize :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onResize = PListener "resize"

onReset :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onReset = PListener "reset"

onScroll :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onScroll = PListener "scroll"

onSearch :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onSearch = PListener "search"

onSeeked :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onSeeked = PListener "seeked"

onSeeking :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onSeeking = PListener "seeking"

onSelect :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onSelect = PListener "select"

onShow :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onShow = PListener "show"

onStalled :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onStalled = PListener "stalled"

onStorage :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onStorage = PListener "storage"

onSubmit :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onSubmit = PListener "submit"

onSuspend :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onSuspend = PListener "suspend"

onTimeupdate :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onTimeupdate = PListener "timeupdate"

onToggle :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onToggle = PListener "toggle"

onTouchcancel :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onTouchcancel = PListener "touchcancel"

onTouchend :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onTouchend = PListener "touchend"

onTouchmove :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onTouchmove = PListener "touchmove"

onTouchstart :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onTouchstart = PListener "touchstart"

onTransitionend :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onTransitionend = PListener "transitionend"

onUnload :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onUnload = PListener "unload"

onVolumechange :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onVolumechange = PListener "volumechange"

onWaiting :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onWaiting = PListener "waiting"

onWheel :: forall m s. (DOMEvent -> Mation m s) -> Prop m s
onWheel = PListener "wheel"
