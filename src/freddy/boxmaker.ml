let isainb a b =
  let ((axl,axh),(ayl,ayh)) = a and
  ((bxl,bxh),(byl,byh)) = b in
  axl <= bxl && axh <= bxh && ayl <= byl && ayh <= byh
  
