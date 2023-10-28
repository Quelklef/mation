-- | This module contains generated code relating to HTML events.
-- |
-- | Names are converted from their canonical `kebab-case` form into `camelCase`, which is idiomatic for Purescript. For example, `align-content` becomes `alignContent`. Purescript-reserved words are suffixed by an understore, so `type` becomes `type_`.

module Mation.Gen.Events where

import Data.Unit (Unit)

import Mation.Core.Dom (DomEvent)
import Mation.Core.Prop (Prop, mkListener)


-- | [HTML abort event](https://developer.mozilla.org/en-US/docs/Web/API/Element/abort_event). This is generated code.
onAbort :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onAbort = mkListener "abort"

-- | [HTML afterprint event](https://developer.mozilla.org/en-US/docs/Web/API/Element/afterprint_event). This is generated code.
onAfterprint :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onAfterprint = mkListener "afterprint"

-- | [HTML animationend event](https://developer.mozilla.org/en-US/docs/Web/API/Element/animationend_event). This is generated code.
onAnimationend :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onAnimationend = mkListener "animationend"

-- | [HTML animationiteration event](https://developer.mozilla.org/en-US/docs/Web/API/Element/animationiteration_event). This is generated code.
onAnimationiteration :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onAnimationiteration = mkListener "animationiteration"

-- | [HTML animationstart event](https://developer.mozilla.org/en-US/docs/Web/API/Element/animationstart_event). This is generated code.
onAnimationstart :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onAnimationstart = mkListener "animationstart"

-- | [HTML beforeprint event](https://developer.mozilla.org/en-US/docs/Web/API/Element/beforeprint_event). This is generated code.
onBeforeprint :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onBeforeprint = mkListener "beforeprint"

-- | [HTML beforeunload event](https://developer.mozilla.org/en-US/docs/Web/API/Element/beforeunload_event). This is generated code.
onBeforeunload :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onBeforeunload = mkListener "beforeunload"

-- | [HTML blur event](https://developer.mozilla.org/en-US/docs/Web/API/Element/blur_event). This is generated code.
onBlur :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onBlur = mkListener "blur"

-- | [HTML canplay event](https://developer.mozilla.org/en-US/docs/Web/API/Element/canplay_event). This is generated code.
onCanplay :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onCanplay = mkListener "canplay"

-- | [HTML canplaythrough event](https://developer.mozilla.org/en-US/docs/Web/API/Element/canplaythrough_event). This is generated code.
onCanplaythrough :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onCanplaythrough = mkListener "canplaythrough"

-- | [HTML change event](https://developer.mozilla.org/en-US/docs/Web/API/Element/change_event). This is generated code.
onChange :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onChange = mkListener "change"

-- | [HTML click event](https://developer.mozilla.org/en-US/docs/Web/API/Element/click_event). This is generated code.
onClick :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onClick = mkListener "click"

-- | [HTML contextmenu event](https://developer.mozilla.org/en-US/docs/Web/API/Element/contextmenu_event). This is generated code.
onContextmenu :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onContextmenu = mkListener "contextmenu"

-- | [HTML copy event](https://developer.mozilla.org/en-US/docs/Web/API/Element/copy_event). This is generated code.
onCopy :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onCopy = mkListener "copy"

-- | [HTML cut event](https://developer.mozilla.org/en-US/docs/Web/API/Element/cut_event). This is generated code.
onCut :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onCut = mkListener "cut"

-- | [HTML dblclick event](https://developer.mozilla.org/en-US/docs/Web/API/Element/dblclick_event). This is generated code.
onDblclick :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onDblclick = mkListener "dblclick"

-- | [HTML drag event](https://developer.mozilla.org/en-US/docs/Web/API/Element/drag_event). This is generated code.
onDrag :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onDrag = mkListener "drag"

-- | [HTML dragend event](https://developer.mozilla.org/en-US/docs/Web/API/Element/dragend_event). This is generated code.
onDragend :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onDragend = mkListener "dragend"

-- | [HTML dragenter event](https://developer.mozilla.org/en-US/docs/Web/API/Element/dragenter_event). This is generated code.
onDragenter :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onDragenter = mkListener "dragenter"

-- | [HTML dragleave event](https://developer.mozilla.org/en-US/docs/Web/API/Element/dragleave_event). This is generated code.
onDragleave :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onDragleave = mkListener "dragleave"

-- | [HTML dragover event](https://developer.mozilla.org/en-US/docs/Web/API/Element/dragover_event). This is generated code.
onDragover :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onDragover = mkListener "dragover"

-- | [HTML dragstart event](https://developer.mozilla.org/en-US/docs/Web/API/Element/dragstart_event). This is generated code.
onDragstart :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onDragstart = mkListener "dragstart"

-- | [HTML drop event](https://developer.mozilla.org/en-US/docs/Web/API/Element/drop_event). This is generated code.
onDrop :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onDrop = mkListener "drop"

-- | [HTML durationchange event](https://developer.mozilla.org/en-US/docs/Web/API/Element/durationchange_event). This is generated code.
onDurationchange :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onDurationchange = mkListener "durationchange"

-- | [HTML ended event](https://developer.mozilla.org/en-US/docs/Web/API/Element/ended_event). This is generated code.
onEnded :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onEnded = mkListener "ended"

-- | [HTML error event](https://developer.mozilla.org/en-US/docs/Web/API/Element/error_event). This is generated code.
onError :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onError = mkListener "error"

-- | [HTML focus event](https://developer.mozilla.org/en-US/docs/Web/API/Element/focus_event). This is generated code.
onFocus :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onFocus = mkListener "focus"

-- | [HTML focusin event](https://developer.mozilla.org/en-US/docs/Web/API/Element/focusin_event). This is generated code.
onFocusin :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onFocusin = mkListener "focusin"

-- | [HTML focusout event](https://developer.mozilla.org/en-US/docs/Web/API/Element/focusout_event). This is generated code.
onFocusout :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onFocusout = mkListener "focusout"

-- | [HTML fullscreenchange event](https://developer.mozilla.org/en-US/docs/Web/API/Element/fullscreenchange_event). This is generated code.
onFullscreenchange :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onFullscreenchange = mkListener "fullscreenchange"

-- | [HTML fullscreenerror event](https://developer.mozilla.org/en-US/docs/Web/API/Element/fullscreenerror_event). This is generated code.
onFullscreenerror :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onFullscreenerror = mkListener "fullscreenerror"

-- | [HTML hashchange event](https://developer.mozilla.org/en-US/docs/Web/API/Element/hashchange_event). This is generated code.
onHashchange :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onHashchange = mkListener "hashchange"

-- | [HTML input event](https://developer.mozilla.org/en-US/docs/Web/API/Element/input_event). This is generated code.
onInput :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onInput = mkListener "input"

-- | [HTML invalid event](https://developer.mozilla.org/en-US/docs/Web/API/Element/invalid_event). This is generated code.
onInvalid :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onInvalid = mkListener "invalid"

-- | [HTML keydown event](https://developer.mozilla.org/en-US/docs/Web/API/Element/keydown_event). This is generated code.
onKeydown :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onKeydown = mkListener "keydown"

-- | [HTML keypress event](https://developer.mozilla.org/en-US/docs/Web/API/Element/keypress_event). This is generated code.
onKeypress :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onKeypress = mkListener "keypress"

-- | [HTML keyup event](https://developer.mozilla.org/en-US/docs/Web/API/Element/keyup_event). This is generated code.
onKeyup :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onKeyup = mkListener "keyup"

-- | [HTML load event](https://developer.mozilla.org/en-US/docs/Web/API/Element/load_event). This is generated code.
onLoad :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onLoad = mkListener "load"

-- | [HTML loadeddata event](https://developer.mozilla.org/en-US/docs/Web/API/Element/loadeddata_event). This is generated code.
onLoadeddata :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onLoadeddata = mkListener "loadeddata"

-- | [HTML loadedmetadata event](https://developer.mozilla.org/en-US/docs/Web/API/Element/loadedmetadata_event). This is generated code.
onLoadedmetadata :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onLoadedmetadata = mkListener "loadedmetadata"

-- | [HTML loadstart event](https://developer.mozilla.org/en-US/docs/Web/API/Element/loadstart_event). This is generated code.
onLoadstart :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onLoadstart = mkListener "loadstart"

-- | [HTML message event](https://developer.mozilla.org/en-US/docs/Web/API/Element/message_event). This is generated code.
onMessage :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onMessage = mkListener "message"

-- | [HTML mousedown event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mousedown_event). This is generated code.
onMousedown :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onMousedown = mkListener "mousedown"

-- | [HTML mouseenter event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseenter_event). This is generated code.
onMouseenter :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onMouseenter = mkListener "mouseenter"

-- | [HTML mouseleave event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseleave_event). This is generated code.
onMouseleave :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onMouseleave = mkListener "mouseleave"

-- | [HTML mousemove event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mousemove_event). This is generated code.
onMousemove :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onMousemove = mkListener "mousemove"

-- | [HTML mouseover event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseover_event). This is generated code.
onMouseover :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onMouseover = mkListener "mouseover"

-- | [HTML mouseout event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseout_event). This is generated code.
onMouseout :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onMouseout = mkListener "mouseout"

-- | [HTML mouseup event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseup_event). This is generated code.
onMouseup :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onMouseup = mkListener "mouseup"

-- | [HTML mousewheel event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mousewheel_event). This is generated code.
onMousewheel :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onMousewheel = mkListener "mousewheel"

-- | [HTML offline event](https://developer.mozilla.org/en-US/docs/Web/API/Element/offline_event). This is generated code.
onOffline :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onOffline = mkListener "offline"

-- | [HTML online event](https://developer.mozilla.org/en-US/docs/Web/API/Element/online_event). This is generated code.
onOnline :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onOnline = mkListener "online"

-- | [HTML open event](https://developer.mozilla.org/en-US/docs/Web/API/Element/open_event). This is generated code.
onOpen :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onOpen = mkListener "open"

-- | [HTML pagehide event](https://developer.mozilla.org/en-US/docs/Web/API/Element/pagehide_event). This is generated code.
onPagehide :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onPagehide = mkListener "pagehide"

-- | [HTML pageshow event](https://developer.mozilla.org/en-US/docs/Web/API/Element/pageshow_event). This is generated code.
onPageshow :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onPageshow = mkListener "pageshow"

-- | [HTML paste event](https://developer.mozilla.org/en-US/docs/Web/API/Element/paste_event). This is generated code.
onPaste :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onPaste = mkListener "paste"

-- | [HTML pause event](https://developer.mozilla.org/en-US/docs/Web/API/Element/pause_event). This is generated code.
onPause :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onPause = mkListener "pause"

-- | [HTML play event](https://developer.mozilla.org/en-US/docs/Web/API/Element/play_event). This is generated code.
onPlay :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onPlay = mkListener "play"

-- | [HTML playing event](https://developer.mozilla.org/en-US/docs/Web/API/Element/playing_event). This is generated code.
onPlaying :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onPlaying = mkListener "playing"

-- | [HTML popstate event](https://developer.mozilla.org/en-US/docs/Web/API/Element/popstate_event). This is generated code.
onPopstate :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onPopstate = mkListener "popstate"

-- | [HTML progress event](https://developer.mozilla.org/en-US/docs/Web/API/Element/progress_event). This is generated code.
onProgress :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onProgress = mkListener "progress"

-- | [HTML ratechange event](https://developer.mozilla.org/en-US/docs/Web/API/Element/ratechange_event). This is generated code.
onRatechange :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onRatechange = mkListener "ratechange"

-- | [HTML resize event](https://developer.mozilla.org/en-US/docs/Web/API/Element/resize_event). This is generated code.
onResize :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onResize = mkListener "resize"

-- | [HTML reset event](https://developer.mozilla.org/en-US/docs/Web/API/Element/reset_event). This is generated code.
onReset :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onReset = mkListener "reset"

-- | [HTML scroll event](https://developer.mozilla.org/en-US/docs/Web/API/Element/scroll_event). This is generated code.
onScroll :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onScroll = mkListener "scroll"

-- | [HTML search event](https://developer.mozilla.org/en-US/docs/Web/API/Element/search_event). This is generated code.
onSearch :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onSearch = mkListener "search"

-- | [HTML seeked event](https://developer.mozilla.org/en-US/docs/Web/API/Element/seeked_event). This is generated code.
onSeeked :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onSeeked = mkListener "seeked"

-- | [HTML seeking event](https://developer.mozilla.org/en-US/docs/Web/API/Element/seeking_event). This is generated code.
onSeeking :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onSeeking = mkListener "seeking"

-- | [HTML select event](https://developer.mozilla.org/en-US/docs/Web/API/Element/select_event). This is generated code.
onSelect :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onSelect = mkListener "select"

-- | [HTML show event](https://developer.mozilla.org/en-US/docs/Web/API/Element/show_event). This is generated code.
onShow :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onShow = mkListener "show"

-- | [HTML stalled event](https://developer.mozilla.org/en-US/docs/Web/API/Element/stalled_event). This is generated code.
onStalled :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onStalled = mkListener "stalled"

-- | [HTML storage event](https://developer.mozilla.org/en-US/docs/Web/API/Element/storage_event). This is generated code.
onStorage :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onStorage = mkListener "storage"

-- | [HTML submit event](https://developer.mozilla.org/en-US/docs/Web/API/Element/submit_event). This is generated code.
onSubmit :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onSubmit = mkListener "submit"

-- | [HTML suspend event](https://developer.mozilla.org/en-US/docs/Web/API/Element/suspend_event). This is generated code.
onSuspend :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onSuspend = mkListener "suspend"

-- | [HTML timeupdate event](https://developer.mozilla.org/en-US/docs/Web/API/Element/timeupdate_event). This is generated code.
onTimeupdate :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onTimeupdate = mkListener "timeupdate"

-- | [HTML toggle event](https://developer.mozilla.org/en-US/docs/Web/API/Element/toggle_event). This is generated code.
onToggle :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onToggle = mkListener "toggle"

-- | [HTML touchcancel event](https://developer.mozilla.org/en-US/docs/Web/API/Element/touchcancel_event). This is generated code.
onTouchcancel :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onTouchcancel = mkListener "touchcancel"

-- | [HTML touchend event](https://developer.mozilla.org/en-US/docs/Web/API/Element/touchend_event). This is generated code.
onTouchend :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onTouchend = mkListener "touchend"

-- | [HTML touchmove event](https://developer.mozilla.org/en-US/docs/Web/API/Element/touchmove_event). This is generated code.
onTouchmove :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onTouchmove = mkListener "touchmove"

-- | [HTML touchstart event](https://developer.mozilla.org/en-US/docs/Web/API/Element/touchstart_event). This is generated code.
onTouchstart :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onTouchstart = mkListener "touchstart"

-- | [HTML transitionend event](https://developer.mozilla.org/en-US/docs/Web/API/Element/transitionend_event). This is generated code.
onTransitionend :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onTransitionend = mkListener "transitionend"

-- | [HTML unload event](https://developer.mozilla.org/en-US/docs/Web/API/Element/unload_event). This is generated code.
onUnload :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onUnload = mkListener "unload"

-- | [HTML volumechange event](https://developer.mozilla.org/en-US/docs/Web/API/Element/volumechange_event). This is generated code.
onVolumechange :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onVolumechange = mkListener "volumechange"

-- | [HTML waiting event](https://developer.mozilla.org/en-US/docs/Web/API/Element/waiting_event). This is generated code.
onWaiting :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onWaiting = mkListener "waiting"

-- | [HTML wheel event](https://developer.mozilla.org/en-US/docs/Web/API/Element/wheel_event). This is generated code.
onWheel :: forall m k. (DomEvent -> k -> m Unit) -> Prop m k
onWheel = mkListener "wheel"

