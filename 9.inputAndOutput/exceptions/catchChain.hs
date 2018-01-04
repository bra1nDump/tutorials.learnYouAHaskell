main = do
  toTry `catch` handler1
  thenTryThis `catch` handler2
  launchRockets
