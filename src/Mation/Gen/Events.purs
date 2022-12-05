module Mation.Gen.Events where

import Prelude
import Mation.Core.Mation (Mation)
import Mation.Core.Dom (DomEvent)
import Mation.Core.Html (Prop, mkListener)


onAbort :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onAbort = mkListener "abort"

onAfterprint :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onAfterprint = mkListener "afterprint"

onAnimationend :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onAnimationend = mkListener "animationend"

onAnimationiteration :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onAnimationiteration = mkListener "animationiteration"

onAnimationstart :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onAnimationstart = mkListener "animationstart"

onBeforeprint :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onBeforeprint = mkListener "beforeprint"

onBeforeunload :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onBeforeunload = mkListener "beforeunload"

onBlur :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onBlur = mkListener "blur"

onCanplay :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onCanplay = mkListener "canplay"

onCanplaythrough :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onCanplaythrough = mkListener "canplaythrough"

onChange :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onChange = mkListener "change"

onClick :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onClick = mkListener "click"

onContextmenu :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onContextmenu = mkListener "contextmenu"

onCopy :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onCopy = mkListener "copy"

onCut :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onCut = mkListener "cut"

onDblclick :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDblclick = mkListener "dblclick"

onDrag :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDrag = mkListener "drag"

onDragend :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDragend = mkListener "dragend"

onDragenter :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDragenter = mkListener "dragenter"

onDragleave :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDragleave = mkListener "dragleave"

onDragover :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDragover = mkListener "dragover"

onDragstart :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDragstart = mkListener "dragstart"

onDrop :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDrop = mkListener "drop"

onDurationchange :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDurationchange = mkListener "durationchange"

onEnded :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onEnded = mkListener "ended"

onError :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onError = mkListener "error"

onFocus :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onFocus = mkListener "focus"

onFocusin :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onFocusin = mkListener "focusin"

onFocusout :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onFocusout = mkListener "focusout"

onFullscreenchange :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onFullscreenchange = mkListener "fullscreenchange"

onFullscreenerror :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onFullscreenerror = mkListener "fullscreenerror"

onHashchange :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onHashchange = mkListener "hashchange"

onInput :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onInput = mkListener "input"

onInvalid :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onInvalid = mkListener "invalid"

onKeydown :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onKeydown = mkListener "keydown"

onKeypress :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onKeypress = mkListener "keypress"

onKeyup :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onKeyup = mkListener "keyup"

onLoad :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onLoad = mkListener "load"

onLoadeddata :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onLoadeddata = mkListener "loadeddata"

onLoadedmetadata :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onLoadedmetadata = mkListener "loadedmetadata"

onLoadstart :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onLoadstart = mkListener "loadstart"

onMessage :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMessage = mkListener "message"

onMousedown :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMousedown = mkListener "mousedown"

onMouseenter :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMouseenter = mkListener "mouseenter"

onMouseleave :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMouseleave = mkListener "mouseleave"

onMousemove :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMousemove = mkListener "mousemove"

onMouseover :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMouseover = mkListener "mouseover"

onMouseout :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMouseout = mkListener "mouseout"

onMouseup :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMouseup = mkListener "mouseup"

onMousewheel :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMousewheel = mkListener "mousewheel"

onOffline :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onOffline = mkListener "offline"

onOnline :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onOnline = mkListener "online"

onOpen :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onOpen = mkListener "open"

onPagehide :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onPagehide = mkListener "pagehide"

onPageshow :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onPageshow = mkListener "pageshow"

onPaste :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onPaste = mkListener "paste"

onPause :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onPause = mkListener "pause"

onPlay :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onPlay = mkListener "play"

onPlaying :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onPlaying = mkListener "playing"

onPopstate :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onPopstate = mkListener "popstate"

onProgress :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onProgress = mkListener "progress"

onRatechange :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onRatechange = mkListener "ratechange"

onResize :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onResize = mkListener "resize"

onReset :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onReset = mkListener "reset"

onScroll :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onScroll = mkListener "scroll"

onSearch :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onSearch = mkListener "search"

onSeeked :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onSeeked = mkListener "seeked"

onSeeking :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onSeeking = mkListener "seeking"

onSelect :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onSelect = mkListener "select"

onShow :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onShow = mkListener "show"

onStalled :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onStalled = mkListener "stalled"

onStorage :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onStorage = mkListener "storage"

onSubmit :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onSubmit = mkListener "submit"

onSuspend :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onSuspend = mkListener "suspend"

onTimeupdate :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onTimeupdate = mkListener "timeupdate"

onToggle :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onToggle = mkListener "toggle"

onTouchcancel :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onTouchcancel = mkListener "touchcancel"

onTouchend :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onTouchend = mkListener "touchend"

onTouchmove :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onTouchmove = mkListener "touchmove"

onTouchstart :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onTouchstart = mkListener "touchstart"

onTransitionend :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onTransitionend = mkListener "transitionend"

onUnload :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onUnload = mkListener "unload"

onVolumechange :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onVolumechange = mkListener "volumechange"

onWaiting :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onWaiting = mkListener "waiting"

onWheel :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onWheel = mkListener "wheel"

