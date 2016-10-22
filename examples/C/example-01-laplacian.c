/*===-- examples/C/example-01-laplacian.c ---------------------------------------------*- C -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 * This example demonstrates how to setup a Serializer, add global meta-information, register
 * fields and savepoints and serialize/deserialize C arrays using the C interface of Serialbox.
 *
 * In this small example we will repeatedly apply a two dimensional laplacian stencil to an input
 * field `phi`. Before and after each invocation of the laplacian stencil, we will serialize the
 * data to disk.
 *
 * This example is also available in all other languages supported by Serialbox.
 *
\*===------------------------------------------------------------------------------------------===*/

/*
 * To use the C API of serialbox, you need to include the serialbox-c/Serialbox.h header
 */
#include "serialbox-c/Serialbox.h"
#include <stdio.h>
#include <stdlib.h>

static const int N = 10;
static const int M = 10;

/*
 * Compute the 2D laplacian on the inner domain of field `phi` and return result in `lap`
 */
static void laplacianStencil(const double* phi, double* lap, int N, int M) {
  int i, j;
  for(i = 0; i < N; ++i)
    for(j = 0; j < M; ++j)
      lap[i * N + j] = phi[(i + 1) * N + j] + phi[(i - 1) * N + j] + phi[i * N + (j + 1)] +
                       phi[i * N + (j - 1)] - 4 * phi[i * N + j];
}

/*===------------------------------------------------------------------------------------------===*\
 *  write()
 *
 * In this function we first prepare the Serializer for writing, add some global meta-information
 * and register the fields `phi` and `lap`. Later, we apply the `laplacianStencil` to `phi` and
 * `lap` and serialize every iteration `phi` as an input and `lap` as an output of the stencil.
 *
\*===------------------------------------------------------------------------------------------===*/
static void write() {
  int i, j, t;

  /*
   * Create a Serializer for writing. Besides the open-policy we have to specify the `directory`
   * in which the Serializer is created and the `prefix` of all files. In case the directory does
   * not exist, it will be created. In addition, if the directory is not empty, all fields with the
   * same `prefix` will be erased (this behaviour can be inhibited using the Append mode).
   */
  serialboxSerializer_t* serializer;
  serializer = serialboxSerializerCreate(Write, "./laplacian/", "field", "Binary");

  /*
   * Allocate the 2D arrays phi and lap and fill it with some random numbers
   */
  double* phi = malloc(N * M * sizeof(double));
  double* lap = malloc(N * M * sizeof(double));
  for(i = 0; i < N; ++i)
    for(j = 0; j < M; ++j) {
      phi[i * N + j] = (double)rand() / RAND_MAX;
      lap[i * N + j] = 0.0;
    }

  /*
   * Create the field meta-information of `phi` and `lap` ...
   */
  int dims[] = {N, M};
  serialboxFieldMetaInfo_t* phiFieldMetaInfo = serialboxFieldMetaInfoCreate(Float64, dims, 2);
  serialboxFieldMetaInfo_t* lapFieldMetaInfo = serialboxFieldMetaInfoCreate(Float64, dims, 2);

  /*
   *  ... and register them within the Serializer ...
   */
  serialboxSerializerAddField(serializer, "phi", phiFieldMetaInfo);
  serialboxSerializerAddField(serializer, "lap", lapFieldMetaInfo);

  /*
   * ... and deallocate the field meta-information objects
   */
  serialboxFieldMetaInfoDestroy(phiFieldMetaInfo);
  serialboxFieldMetaInfoDestroy(lapFieldMetaInfo);

  /*
   * Next, we add some global meta-information to the serializer. Besides the usual `key = value`
   * pair, you can also add `key = {value1, ..., valueN}` pairs. Note that the same API can also be
   * used to meta-information to the fields.
   * We first obtain a refrence to the global meta-information of the serializer ...
   */
  serialboxMetaInfo_t* globalMetaInfo = serialboxSerializerGetGlobalMetaInfo(serializer);

  /*
   * ... add our meta-information in the form of an int ...
   */
  serialboxMetaInfoAddInt32(globalMetaInfo, "answer", 42);

  /*
   * ... an array of ints (halos) ...
   */
  serialboxArrayOfInt32_t* haloArray = serialboxArrayOfInt32Create(4);
  haloArray->data[0] = haloArray->data[1] = haloArray->data[2] = haloArray->data[3] = 1;

  serialboxMetaInfoAddArrayOfInt32(globalMetaInfo, "halos", haloArray);

  serialboxArrayOfInt32Destroy(haloArray);

  /*
   * ... and finally deallocate the meta-info reference
   */
  serialboxMetaInfoDestroy(globalMetaInfo);

  /*
   * Up to this point nothing has been written to disk. Using update_meta_data() will force a write
   * all meta-information to the corresponding JSON files. Note that the meta-data is updated after
   * each call and thus a manual update of the meta-data is seldom required. If you are curious you
   * can inspect the files './laplacian/MetaData-field.json' and
   * './laplacian/ArchiveMetaData-field.json'
   */
  serialboxSerializerUpdateMetaData(serializer);

  /*
   * We now apply the `laplacian_stencil` three times to phi. In each iteration we will create an
   * input and output savepoint where we save the current `phi` field (input) and `lap` field
   * (output)
   */
  for(t = 0; t < 3; ++t) {
    /*
     * Create a Savepoint. Savepoints can have the same name as long as they have different
     * meta-information. In our case we will always store the current time step `t` as a
     * meta-information, thus making it unique. Note that the procedure to add meta-information
     * follows the same pattern as with adding global meta-information to the Serializer: Obtain a
     * reference to the meta-info object, add the information, destroy the reference.
     */
    serialboxSavepoint_t* savepoint_in = serialboxSavepointCreate("laplacian-in");
    serialboxMetaInfo_t* metaInfo_in = serialboxSavepointGetMetaInfo(savepoint_in);
    serialboxMetaInfoAddInt32(metaInfo_in, "time", t);
    serialboxMetaInfoDestroy(metaInfo_in);

    /*
     * Register the Savepoint.
     */
    serialboxSerializerAddSavepoint(serializer, savepoint_in);

    /*
     * Write phi to disk at our input savepoint. This will create the file `field_phi.dat` upon
     * first invocation and afterwards the data is appended.
     */
    int strides[2] = {N, 1};
    serialboxSerializerWrite(serializer, "phi", savepoint_in, phi, strides, 2);

    /*
     * Apply the laplacian_stencil to phi
     */
    laplacianStencil(phi, lap, N, N);

    /*
     * Create the output savepoint.
     */
    serialboxSavepoint_t* savepoint_out = serialboxSavepointCreate("laplacian-out");
    serialboxMetaInfo_t* metaInfo_out = serialboxSavepointGetMetaInfo(savepoint_out);
    serialboxMetaInfoAddInt32(metaInfo_out, "time", t);
    serialboxMetaInfoDestroy(metaInfo_out);

    /*
     * Write lap to disk. Note that here we implicitly register the output savepoint.
     */
    serialboxSerializerWrite(serializer, "lap", savepoint_out, lap, strides, 2);

    /*
     * Finally, we swap phi with lap
     */
    double* tmp = phi;
    phi = lap;
    lap = tmp;
  }

  /*
   * The last thing we need to do, is to free the serializer and all additionally allocated memory.
   */
  serialboxSerializerDestroy(serializer);
  free(lap);
  free(phi);
}

/*===------------------------------------------------------------------------------------------===*\
 *  read()
 *
 * In this function we initialize the Serializer for reading with our serialized data from the 
 * write() method. First, we query some meta-data, like the global meta-information, the 
 * dimensions of field `phi` or the vector of savepoints. 
 * Afterwards, we apply the same three time steps of the `laplacianStencil` to `phi` to compute 
 * `lap` but this time we compare the result (i.e the content of `lap`) to the  to the reference 
 * loaded from disk `lap_reference` which we computed in the write() method. Obviously, the results 
 * will match as we apply the exact same stencil but in a real world scenario you might use a 
 * different implementations of the stencil and this is where Serialbox has it's use case. 
 *
\*===------------------------------------------------------------------------------------------===*/
static void read() {
  int i, j, t;

  /*
   * Create a Serializer for reading. This gives access to the previously written data.
   */
  serialboxSerializer_t* serializer;
  serializer = serialboxSerializerCreate(Read, "./laplacian/", "field", "Binary");

  /*
   * Allocate the 2D arrays phi and lap
   */
  double* phi = malloc(N * M * sizeof(double));
  double* lap = malloc(N * M * sizeof(double));
  double* lap_reference = malloc(N * M * sizeof(double));

  /*
   * We first obtain a refrence to the global meta-information of the serializer ...
   */
  serialboxMetaInfo_t* globalMetaInfo = serialboxSerializerGetGlobalMetaInfo(serializer);

  /*
   * ... get the meta-information of "answer" as an int ...
   */
  int answer = serialboxMetaInfoGetInt32(globalMetaInfo, "answer");
  printf("The answer is %i\n", answer);

  /*
   * .. and the "halos" as an array of ints (halos) ...
   */
  serialboxArrayOfInt32_t* halos = serialboxMetaInfoGetArrayOfInt32(globalMetaInfo, "halos");
  printf("The halo boundaries are [ ");
  for(i = 0; i < halos->len; ++i)
    printf("%i ", halos->data[i]);
  puts("]");
  serialboxArrayOfInt32Destroy(halos);

  /*
   * ... and finally deallocate the meta-info refrence
   */
  serialboxMetaInfoDestroy(globalMetaInfo);

  /*
   * Access the registered fields (Note that the individual elements (i.e char*) need to be freed
   * manually).
   */
  serialboxArrayOfString_t* fieldnames = serialboxSerializerGetFieldnames(serializer);

  printf("The registered fields are: [ ");
  for(i = 0; i < fieldnames->len; ++i) {
    printf("'%s' ", fieldnames->data[i]);
    free(fieldnames->data[i]);
  }
  puts("]");

  serialboxArrayOfStringDestroy(fieldnames);

  /*
   * Access the dimensions of phi via the field meta-information.
   */
  serialboxFieldMetaInfo_t* info = serialboxSerializerGetFieldMetaInfo(serializer, "phi");

  int numDims = serialboxFieldMetaInfoGetNumDimensions(info);
  const int* dims = serialboxFieldMetaInfoGetDimensions(info);

  printf("Dimensions of phi: [ ");
  for(i = 0; i < numDims; ++i)
    printf("%i ", dims[i]);
  puts("]");

  /*
   * Access the savepoints. The savepoints are ordered in the order they were inserted.
   */
  int numSavepoints = serialboxSerializerGetNumSavepoints(serializer);
  serialboxSavepoint_t** savepoints = serialboxSerializerGetSavepointVector(serializer);

  puts("Savepoints:");
  for(i = 0; i < numSavepoints; ++i)
    printf(" %s\n", serialboxSavepointToString(savepoints[i]));

  /*
   * We will now perform the same iterations as in the write method but this time we will read
   * phi as an input from disk, compute the laplacian and compare the result to the stored output
   * of lap on disk (loaded as `lap_reference`).
   */
  for(t = 0; t < 3; ++t) {
    /*
     * Get the current input savepoint at time t (the factor of 2 is due to the fact that we
     * stored input and output in alternating order).
     */
    serialboxSavepoint_t* savepoint_in = savepoints[2 * t];

    /*
     * Load phi from disk.
     */
    int strides[2] = {N, 1};
    serialboxSerializerRead(serializer, "phi", savepoint_in, phi, strides, 2);

    /*
     * Apply the laplacian_stencil to phi
     */
    laplacianStencil(phi, lap, N, N);

    /*
     * Load the reference output of lap ...
     */
    serialboxSavepoint_t* savepoint_out = savepoints[2 * t + 1];
    serialboxSerializerRead(serializer, "lap", savepoint_out, lap_reference, strides, 2);

    /*
     * ... and compare the results on the inner domain.
     */
    for(i = 1; i < N - 1; ++i)
      for(j = 1; j < M - 1; ++j)
        if(lap[i * N + j] != lap_reference[i * N + j])
          printf("ERROR: at (%i,%i) of lap and lap_refrence: %f vs. %f\n", i, j, lap[i * N + j],
                 lap_reference[i * N + j]);
  }

  /*
   * The last thing we need to do, is to free the serializer and the savepoint vector.
   */
  serialboxSerializerDestroySavepointVector(savepoints, numSavepoints);
  serialboxSerializerDestroy(serializer);
  free(lap);
  free(lap_reference);
  free(phi);
}

/*===------------------------------------------------------------------------------------------===*\
 *  main()
 *
 * Here we call our write() and read() functions.
 *
\*===------------------------------------------------------------------------------------------===*/
int main() {
  
  write();

  read();

  return 0;
}
