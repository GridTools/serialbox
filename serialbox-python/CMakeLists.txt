# TODO I just copy snippets to this location from the old main CMakeLists.txt


#---------------------------------------- Python ---------------------------------------------------
if(SERIALBOX_ENABLE_PYTHON)
  find_package(PythonInterp 3.4)
  
  # Python tests are enabled by default if we can find "nose" and "numpy"
  set(ENABLE_PYTHON_TESTS OFF)
  if(SERIALBOX_TESTING AND PYTHONINTERP_FOUND)
    include(FindPythonModule)
    find_python_module(nose)
    find_python_module(numpy)
    if(PY_NOSE_FOUND AND PY_NUMPY_FOUND)   
      set(ENABLE_PYTHON_TESTS ON)
    endif()
  endif()
  
  option(SERIALBOX_TESTING_PYTHON "Run Python tests" ${ENABLE_PYTHON_TESTS})
  mark_as_advanced(ENABLE_PYTHON_TESTS)
  
endif(SERIALBOX_ENABLE_PYTHON)
  