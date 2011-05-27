ghc --make -fforce-recomp template.hs && runghc inject.hs && chmod +x injected && ./injected
