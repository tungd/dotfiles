local logger = hs.logger.new('init', 'info')

local eventTypes = hs.eventtap.event.types
local eventProps = hs.eventtap.event.properties

-- Fix deadkey 'n' on my internal keyboard
hs.eventtap.new({eventTypes.keyDown}, function(event)
      local keyCode = event:getProperty(eventProps.keyboardEventKeycode)
      local keyboardType = event:getProperty(eventProps.keyboardEventKeyboardType)
      local modifiers = hs.eventtap.checkKeyboardModifiers()

      logger.d(
         "keyDown [keyCode: " .. keyCode ..
            ", keyboardType: " .. keyboardType ..
            -- ", modifiers: " .. modifiers["alt"] ..
            "]")

      -- 43 is the internal keyboard
      if keyboardType == 43 and keyCode == hs.keycodes.map["\\"] then
         local e = event:copy()
         if modifiers["alt"] then
            -- Unless the 'option' key is down, clear the flag so it is still
            -- '\\', but passing through other modifiers such as
            -- 'shift'. Doesn't work well with repeat but it's fine for me
            modifiers["alt"] = nil
            e:setFlags(modifiers)
            return true, {e}
         end

         e:setKeyCode(hs.keycodes.map["n"])
         return true, {e}
      end
end):start()

function focus_prev()
   local windows = hs.window.filter.default:getWindows(
      hs.window.filter.sortByLastFocused
   )
   windows[2]:focus()
end

hs.hotkey.bind({"option"}, "j", function()
      hs.hints.windowHints()
      -- expose:show()
end)
hs.hotkey.bind({"option"}, "k", focus_prev, nil, focus_prev)


hs.hotkey.bind({"option"}, "r", function()
      hs.alert('Reloading')
      hs.timer.delayed.new(0.4, hs.fnutils.partial(hs.reload, self)):start()
end)
