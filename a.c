#include "/Volumes/HDD/Xcode/DerivedData/GameEngineV3-gddihdqoaowbzfawcjmwrkrhegpf/Build/Products/Debug/usr/local/include/Game.hpp"
#include "/Volumes/HDD/Xcode/DerivedData/GameEngineV3-gddihdqoaowbzfawcjmwrkrhegpf/Build/Products/Debug/usr/local/include/Pacman.hpp"

int play() {
  GameEngineInit();
  Window   *main_window = new Window("Game Engine V3", 800,600);
  Renderer2D *renderer    = new Renderer2D(main_window);
  Application.SetWindow(main_window, renderer);
  
  Options.showColliders = true;
  
  //  loadTest(&Application);
  //  loadPokemon(&Application);
  //  LoadSP(&Application);
  //  loadGol();
  //  loadGridTest(&game);
  //  loadChess(&game);
  loadPacman();
  //  loadDraw();
  //  Application.currentScene = shared_ptr<Scene>(new NNCircle);
  
  
  
  Application.setMaxFramesPerSecond(60);
  //  delete game;
  //
  //  Window   *tmain_window = new Window("Game Engine V3", 800,600);
  //  Renderer2D *trenderer    = new Renderer2D(tmain_window);
  //  Game     *tgame        = new Game(tmain_window, trenderer);
  //  loadGame("/Users/carlosspaggiari/Game.xml", game);
  //    thread t (&Server::Start, &s);
  Application.Run();
  //  saveSerialize<Game>("/Users/carlosspaggiari/Game.xml", Application);
  //  loadGame("/Users/carlosspaggiari/Game.xml", game);
  
  
  
  GameEngineQuit();
  
  //  delete game;
  return 0;
}



extern "C" {
int hola(int x){
  play();
  return x;
}

}