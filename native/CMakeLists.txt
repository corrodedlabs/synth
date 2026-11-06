cmake_minimum_required(VERSION 3.22)
project(synth_native C CXX)

# Apsis is a submodule at the repo root. Its examples and tests aren't this game's business.
set(APSIS_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)
set(APSIS_BUILD_TESTS OFF CACHE BOOL "" FORCE)
add_subdirectory(../apsis ${CMAKE_BINARY_DIR}/apsis)

# Apsis pins stb; this picks up the same copy, whose path stays in Apsis's scope otherwise.
FetchContent_GetProperties(stb)

set(warnings -Wall -Wextra -Wno-unused-parameter -Wshadow)

# The table's logic needs no GPU, so its tests need neither one nor a window.
add_library(synth_table STATIC src/table.c)
target_include_directories(synth_table PUBLIC src)
target_link_libraries(synth_table PUBLIC apsis_anim apsis_core PRIVATE m)
target_compile_options(synth_table PRIVATE ${warnings})

# The text on the table. Pinned like every other dependency.
FetchContent_Declare(dejavu
  URL https://github.com/dejavu-fonts/dejavu-fonts/releases/download/version_2_37/dejavu-fonts-ttf-2.37.tar.bz2
  URL_HASH SHA256=fa9ca4d13871dd122f61258a80d01751d603b4d3ee14095d65453b4e846e17d7
  SOURCE_SUBDIR unpack-only)
FetchContent_MakeAvailable(dejavu)
set(font ${dejavu_SOURCE_DIR}/ttf/DejaVuSans.ttf)
set(atlas ${CMAKE_CURRENT_SOURCE_DIR}/assets/cards.png)

set(sources src/main.c src/sound.c src/commands.c src/third_party.c)
if(ANDROID)
  # SDL's Java side loads libmain.so and calls SDL_main. Gradle puts this folder into the APK.
  set(generated ${CMAKE_CURRENT_SOURCE_DIR}/../android/app/build/generated/synth)
  set(shader_dir ${generated}/assets/shaders)
  add_library(game SHARED ${sources} src/jni.c)
  set_target_properties(game PROPERTIES OUTPUT_NAME main)
  target_compile_definitions(game PRIVATE SYNTH_SHADERS="shaders" SYNTH_ASSETS="table")
  file(COPY ${atlas} ${font} DESTINATION ${generated}/assets/table)

  # Validation layers, for debug APKs only.
  FetchContent_Declare(vulkan_validation
    URL https://github.com/KhronosGroup/Vulkan-ValidationLayers/releases/download/vulkan-sdk-1.4.304.0/android-binaries-1.4.304.0.zip
    URL_HASH SHA256=3e67710f93daa7f39823e85b4ba5aaaf6cd44d207747715fa258a6796b9798ed
    SOURCE_SUBDIR unpack-only)
  FetchContent_MakeAvailable(vulkan_validation)
  file(COPY ${vulkan_validation_SOURCE_DIR}/${ANDROID_ABI}/libVkLayer_khronos_validation.so
       DESTINATION ${generated}/layers/${ANDROID_ABI})
else()
  add_executable(game ${sources})
  set_target_properties(game PROPERTIES OUTPUT_NAME synth_table)
  set(shader_dir ${CMAKE_CURRENT_BINARY_DIR}/shaders)
  set(asset_dir ${CMAKE_CURRENT_BINARY_DIR}/assets)
  target_compile_definitions(game PRIVATE SYNTH_SHADERS="${shader_dir}" SYNTH_ASSETS="${asset_dir}")
  file(COPY ${atlas} ${font} DESTINATION ${asset_dir})

  enable_testing()
  add_executable(table_test tests/table_test.c)
  target_link_libraries(table_test PRIVATE synth_table m)
  add_test(NAME table COMMAND table_test)
endif()

set_source_files_properties(src/third_party.c PROPERTIES COMPILE_OPTIONS "-w")
target_include_directories(game PRIVATE ${stb_SOURCE_DIR})
target_link_libraries(game PRIVATE synth_table apsis_platform apsis_simple apsis_anim apsis_audio apsis_gpu apsis_core SDL3::SDL3 m)
target_compile_options(game PRIVATE ${warnings})
# SPIR-V 1.3, which phones on Apsis's Vulkan 1.1 path can load, and 1.3 phones run too.
apsis_add_simple_shaders(game ${shader_dir} TARGET_ENV vulkan1.1)
