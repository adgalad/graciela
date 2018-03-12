

rungraciela --stdc++ prueba.gcl \
  -c a.c \
  -l /Volumes/HDD/Xcode/DerivedData/GameEngineV3-gddihdqoaowbzfawcjmwrkrhegpf/Build/Products/Debug/libGameEngine.a \
  -l /usr/local/Cellar/boost/1.63.0/lib/libboost_serialization.a \
  -DGAME_ENGINE_V3 -DENABLE_BOOST_SERIALIZATION -DENGINE2D \
  -f SDL2 \
  -f SDL2_image \
  -f SDL2_mixer \
  -f SDL2_ttf
