# Show all files in the IDE (useful for QtCreator)
option(IDE_SHOW_ALL_SOURCE_FILES "Show all source files in the IDE" OFF)
if (IDE_SHOW_ALL_SOURCE_FILES)
    file(GLOB_RECURSE ALL_IDE_FILES *.cpp *.hpp *.h *.cmake)
    add_custom_target(sources
        SOURCES
            ${ALL_IDE_FILES})
endif()
