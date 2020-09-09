return {
   expandhome = function (path)
      if path:sub(1, 1) == "~" then
	 return os.getenv("HOME") .. path:sub(2)
      end
      return path
   end,
}
