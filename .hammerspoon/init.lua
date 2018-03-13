local logger = hs.logger.new('init', 'info')

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
