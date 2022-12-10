-- | This module contains generated code relating to HTML events.
-- |
-- | Names are converted from their canonical `kebab-case` form into `camelCase`, which is idiomatic for Purescript. For example, `align-content` becomes `alignContent`. Purescript-reserved words are suffixed by an understore, so `type` becomes `type_`.

module Mation.Gen.Events where

import Prelude
import Mation.Core.Mation (Mation)
import Mation.Core.Dom (DomEvent)
import Mation.Core.Prop (Prop, mkListener)


-- | [HTML abort event](https://developer.mozilla.org/en-US/docs/Web/API/Element/abort_event). This is generated code.
onAbort :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onAbort = mkListener "abort"

-- | [HTML afterprint event](https://developer.mozilla.org/en-US/docs/Web/API/Element/afterprint_event). This is generated code.
onAfterprint :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onAfterprint = mkListener "afterprint"

-- | [HTML animationend event](https://developer.mozilla.org/en-US/docs/Web/API/Element/animationend_event). This is generated code.
onAnimationend :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onAnimationend = mkListener "animationend"

-- | [HTML animationiteration event](https://developer.mozilla.org/en-US/docs/Web/API/Element/animationiteration_event). This is generated code.
onAnimationiteration :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onAnimationiteration = mkListener "animationiteration"

-- | [HTML animationstart event](https://developer.mozilla.org/en-US/docs/Web/API/Element/animationstart_event). This is generated code.
onAnimationstart :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onAnimationstart = mkListener "animationstart"

-- | [HTML beforeprint event](https://developer.mozilla.org/en-US/docs/Web/API/Element/beforeprint_event). This is generated code.
onBeforeprint :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onBeforeprint = mkListener "beforeprint"

-- | [HTML beforeunload event](https://developer.mozilla.org/en-US/docs/Web/API/Element/beforeunload_event). This is generated code.
onBeforeunload :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onBeforeunload = mkListener "beforeunload"

-- | [HTML blur event](https://developer.mozilla.org/en-US/docs/Web/API/Element/blur_event). This is generated code.
onBlur :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onBlur = mkListener "blur"

-- | [HTML canplay event](https://developer.mozilla.org/en-US/docs/Web/API/Element/canplay_event). This is generated code.
onCanplay :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onCanplay = mkListener "canplay"

-- | [HTML canplaythrough event](https://developer.mozilla.org/en-US/docs/Web/API/Element/canplaythrough_event). This is generated code.
onCanplaythrough :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onCanplaythrough = mkListener "canplaythrough"

-- | [HTML change event](https://developer.mozilla.org/en-US/docs/Web/API/Element/change_event). This is generated code.
onChange :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onChange = mkListener "change"

-- | [HTML click event](https://developer.mozilla.org/en-US/docs/Web/API/Element/click_event). This is generated code.
onClick :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onClick = mkListener "click"

-- | [HTML contextmenu event](https://developer.mozilla.org/en-US/docs/Web/API/Element/contextmenu_event). This is generated code.
onContextmenu :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onContextmenu = mkListener "contextmenu"

-- | [HTML copy event](https://developer.mozilla.org/en-US/docs/Web/API/Element/copy_event). This is generated code.
onCopy :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onCopy = mkListener "copy"

-- | [HTML cut event](https://developer.mozilla.org/en-US/docs/Web/API/Element/cut_event). This is generated code.
onCut :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onCut = mkListener "cut"

-- | [HTML dblclick event](https://developer.mozilla.org/en-US/docs/Web/API/Element/dblclick_event). This is generated code.
onDblclick :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDblclick = mkListener "dblclick"

-- | [HTML drag event](https://developer.mozilla.org/en-US/docs/Web/API/Element/drag_event). This is generated code.
onDrag :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDrag = mkListener "drag"

-- | [HTML dragend event](https://developer.mozilla.org/en-US/docs/Web/API/Element/dragend_event). This is generated code.
onDragend :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDragend = mkListener "dragend"

-- | [HTML dragenter event](https://developer.mozilla.org/en-US/docs/Web/API/Element/dragenter_event). This is generated code.
onDragenter :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDragenter = mkListener "dragenter"

-- | [HTML dragleave event](https://developer.mozilla.org/en-US/docs/Web/API/Element/dragleave_event). This is generated code.
onDragleave :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDragleave = mkListener "dragleave"

-- | [HTML dragover event](https://developer.mozilla.org/en-US/docs/Web/API/Element/dragover_event). This is generated code.
onDragover :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDragover = mkListener "dragover"

-- | [HTML dragstart event](https://developer.mozilla.org/en-US/docs/Web/API/Element/dragstart_event). This is generated code.
onDragstart :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDragstart = mkListener "dragstart"

-- | [HTML drop event](https://developer.mozilla.org/en-US/docs/Web/API/Element/drop_event). This is generated code.
onDrop :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDrop = mkListener "drop"

-- | [HTML durationchange event](https://developer.mozilla.org/en-US/docs/Web/API/Element/durationchange_event). This is generated code.
onDurationchange :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onDurationchange = mkListener "durationchange"

-- | [HTML ended event](https://developer.mozilla.org/en-US/docs/Web/API/Element/ended_event). This is generated code.
onEnded :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onEnded = mkListener "ended"

-- | [HTML error event](https://developer.mozilla.org/en-US/docs/Web/API/Element/error_event). This is generated code.
onError :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onError = mkListener "error"

-- | [HTML focus event](https://developer.mozilla.org/en-US/docs/Web/API/Element/focus_event). This is generated code.
onFocus :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onFocus = mkListener "focus"

-- | [HTML focusin event](https://developer.mozilla.org/en-US/docs/Web/API/Element/focusin_event). This is generated code.
onFocusin :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onFocusin = mkListener "focusin"

-- | [HTML focusout event](https://developer.mozilla.org/en-US/docs/Web/API/Element/focusout_event). This is generated code.
onFocusout :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onFocusout = mkListener "focusout"

-- | [HTML fullscreenchange event](https://developer.mozilla.org/en-US/docs/Web/API/Element/fullscreenchange_event). This is generated code.
onFullscreenchange :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onFullscreenchange = mkListener "fullscreenchange"

-- | [HTML fullscreenerror event](https://developer.mozilla.org/en-US/docs/Web/API/Element/fullscreenerror_event). This is generated code.
onFullscreenerror :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onFullscreenerror = mkListener "fullscreenerror"

-- | [HTML hashchange event](https://developer.mozilla.org/en-US/docs/Web/API/Element/hashchange_event). This is generated code.
onHashchange :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onHashchange = mkListener "hashchange"

-- | [HTML input event](https://developer.mozilla.org/en-US/docs/Web/API/Element/input_event). This is generated code.
onInput :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onInput = mkListener "input"

-- | [HTML invalid event](https://developer.mozilla.org/en-US/docs/Web/API/Element/invalid_event). This is generated code.
onInvalid :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onInvalid = mkListener "invalid"

-- | [HTML keydown event](https://developer.mozilla.org/en-US/docs/Web/API/Element/keydown_event). This is generated code.
onKeydown :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onKeydown = mkListener "keydown"

-- | [HTML keypress event](https://developer.mozilla.org/en-US/docs/Web/API/Element/keypress_event). This is generated code.
onKeypress :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onKeypress = mkListener "keypress"

-- | [HTML keyup event](https://developer.mozilla.org/en-US/docs/Web/API/Element/keyup_event). This is generated code.
onKeyup :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onKeyup = mkListener "keyup"

-- | [HTML load event](https://developer.mozilla.org/en-US/docs/Web/API/Element/load_event). This is generated code.
onLoad :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onLoad = mkListener "load"

-- | [HTML loadeddata event](https://developer.mozilla.org/en-US/docs/Web/API/Element/loadeddata_event). This is generated code.
onLoadeddata :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onLoadeddata = mkListener "loadeddata"

-- | [HTML loadedmetadata event](https://developer.mozilla.org/en-US/docs/Web/API/Element/loadedmetadata_event). This is generated code.
onLoadedmetadata :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onLoadedmetadata = mkListener "loadedmetadata"

-- | [HTML loadstart event](https://developer.mozilla.org/en-US/docs/Web/API/Element/loadstart_event). This is generated code.
onLoadstart :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onLoadstart = mkListener "loadstart"

-- | [HTML message event](https://developer.mozilla.org/en-US/docs/Web/API/Element/message_event). This is generated code.
onMessage :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMessage = mkListener "message"

-- | [HTML mousedown event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mousedown_event). This is generated code.
onMousedown :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMousedown = mkListener "mousedown"

-- | [HTML mouseenter event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseenter_event). This is generated code.
onMouseenter :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMouseenter = mkListener "mouseenter"

-- | [HTML mouseleave event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseleave_event). This is generated code.
onMouseleave :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMouseleave = mkListener "mouseleave"

-- | [HTML mousemove event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mousemove_event). This is generated code.
onMousemove :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMousemove = mkListener "mousemove"

-- | [HTML mouseover event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseover_event). This is generated code.
onMouseover :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMouseover = mkListener "mouseover"

-- | [HTML mouseout event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseout_event). This is generated code.
onMouseout :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMouseout = mkListener "mouseout"

-- | [HTML mouseup event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseup_event). This is generated code.
onMouseup :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMouseup = mkListener "mouseup"

-- | [HTML mousewheel event](https://developer.mozilla.org/en-US/docs/Web/API/Element/mousewheel_event). This is generated code.
onMousewheel :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onMousewheel = mkListener "mousewheel"

-- | [HTML offline event](https://developer.mozilla.org/en-US/docs/Web/API/Element/offline_event). This is generated code.
onOffline :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onOffline = mkListener "offline"

-- | [HTML online event](https://developer.mozilla.org/en-US/docs/Web/API/Element/online_event). This is generated code.
onOnline :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onOnline = mkListener "online"

-- | [HTML open event](https://developer.mozilla.org/en-US/docs/Web/API/Element/open_event). This is generated code.
onOpen :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onOpen = mkListener "open"

-- | [HTML pagehide event](https://developer.mozilla.org/en-US/docs/Web/API/Element/pagehide_event). This is generated code.
onPagehide :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onPagehide = mkListener "pagehide"

-- | [HTML pageshow event](https://developer.mozilla.org/en-US/docs/Web/API/Element/pageshow_event). This is generated code.
onPageshow :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onPageshow = mkListener "pageshow"

-- | [HTML paste event](https://developer.mozilla.org/en-US/docs/Web/API/Element/paste_event). This is generated code.
onPaste :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onPaste = mkListener "paste"

-- | [HTML pause event](https://developer.mozilla.org/en-US/docs/Web/API/Element/pause_event). This is generated code.
onPause :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onPause = mkListener "pause"

-- | [HTML play event](https://developer.mozilla.org/en-US/docs/Web/API/Element/play_event). This is generated code.
onPlay :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onPlay = mkListener "play"

-- | [HTML playing event](https://developer.mozilla.org/en-US/docs/Web/API/Element/playing_event). This is generated code.
onPlaying :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onPlaying = mkListener "playing"

-- | [HTML popstate event](https://developer.mozilla.org/en-US/docs/Web/API/Element/popstate_event). This is generated code.
onPopstate :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onPopstate = mkListener "popstate"

-- | [HTML progress event](https://developer.mozilla.org/en-US/docs/Web/API/Element/progress_event). This is generated code.
onProgress :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onProgress = mkListener "progress"

-- | [HTML ratechange event](https://developer.mozilla.org/en-US/docs/Web/API/Element/ratechange_event). This is generated code.
onRatechange :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onRatechange = mkListener "ratechange"

-- | [HTML resize event](https://developer.mozilla.org/en-US/docs/Web/API/Element/resize_event). This is generated code.
onResize :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onResize = mkListener "resize"

-- | [HTML reset event](https://developer.mozilla.org/en-US/docs/Web/API/Element/reset_event). This is generated code.
onReset :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onReset = mkListener "reset"

-- | [HTML scroll event](https://developer.mozilla.org/en-US/docs/Web/API/Element/scroll_event). This is generated code.
onScroll :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onScroll = mkListener "scroll"

-- | [HTML search event](https://developer.mozilla.org/en-US/docs/Web/API/Element/search_event). This is generated code.
onSearch :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onSearch = mkListener "search"

-- | [HTML seeked event](https://developer.mozilla.org/en-US/docs/Web/API/Element/seeked_event). This is generated code.
onSeeked :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onSeeked = mkListener "seeked"

-- | [HTML seeking event](https://developer.mozilla.org/en-US/docs/Web/API/Element/seeking_event). This is generated code.
onSeeking :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onSeeking = mkListener "seeking"

-- | [HTML select event](https://developer.mozilla.org/en-US/docs/Web/API/Element/select_event). This is generated code.
onSelect :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onSelect = mkListener "select"

-- | [HTML show event](https://developer.mozilla.org/en-US/docs/Web/API/Element/show_event). This is generated code.
onShow :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onShow = mkListener "show"

-- | [HTML stalled event](https://developer.mozilla.org/en-US/docs/Web/API/Element/stalled_event). This is generated code.
onStalled :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onStalled = mkListener "stalled"

-- | [HTML storage event](https://developer.mozilla.org/en-US/docs/Web/API/Element/storage_event). This is generated code.
onStorage :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onStorage = mkListener "storage"

-- | [HTML submit event](https://developer.mozilla.org/en-US/docs/Web/API/Element/submit_event). This is generated code.
onSubmit :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onSubmit = mkListener "submit"

-- | [HTML suspend event](https://developer.mozilla.org/en-US/docs/Web/API/Element/suspend_event). This is generated code.
onSuspend :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onSuspend = mkListener "suspend"

-- | [HTML timeupdate event](https://developer.mozilla.org/en-US/docs/Web/API/Element/timeupdate_event). This is generated code.
onTimeupdate :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onTimeupdate = mkListener "timeupdate"

-- | [HTML toggle event](https://developer.mozilla.org/en-US/docs/Web/API/Element/toggle_event). This is generated code.
onToggle :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onToggle = mkListener "toggle"

-- | [HTML touchcancel event](https://developer.mozilla.org/en-US/docs/Web/API/Element/touchcancel_event). This is generated code.
onTouchcancel :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onTouchcancel = mkListener "touchcancel"

-- | [HTML touchend event](https://developer.mozilla.org/en-US/docs/Web/API/Element/touchend_event). This is generated code.
onTouchend :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onTouchend = mkListener "touchend"

-- | [HTML touchmove event](https://developer.mozilla.org/en-US/docs/Web/API/Element/touchmove_event). This is generated code.
onTouchmove :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onTouchmove = mkListener "touchmove"

-- | [HTML touchstart event](https://developer.mozilla.org/en-US/docs/Web/API/Element/touchstart_event). This is generated code.
onTouchstart :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onTouchstart = mkListener "touchstart"

-- | [HTML transitionend event](https://developer.mozilla.org/en-US/docs/Web/API/Element/transitionend_event). This is generated code.
onTransitionend :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onTransitionend = mkListener "transitionend"

-- | [HTML unload event](https://developer.mozilla.org/en-US/docs/Web/API/Element/unload_event). This is generated code.
onUnload :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onUnload = mkListener "unload"

-- | [HTML volumechange event](https://developer.mozilla.org/en-US/docs/Web/API/Element/volumechange_event). This is generated code.
onVolumechange :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onVolumechange = mkListener "volumechange"

-- | [HTML waiting event](https://developer.mozilla.org/en-US/docs/Web/API/Element/waiting_event). This is generated code.
onWaiting :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onWaiting = mkListener "waiting"

-- | [HTML wheel event](https://developer.mozilla.org/en-US/docs/Web/API/Element/wheel_event). This is generated code.
onWheel :: forall m s. (DomEvent -> Mation m s) -> Prop m s
onWheel = mkListener "wheel"

