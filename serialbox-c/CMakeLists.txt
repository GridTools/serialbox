#TODO just snippets

if(SERIALBOX_ENABLE_C)
    install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src/serialbox-c DESTINATION include 
        FILES_MATCHING PATTERN "*.h")
endif()