#include <algorithm>
#include <numeric>
#include <vector>

#include "IR/Type.h"
#include "Utils/Utils.h"

std::vector<size_t> getArrayDimensions(const ir::ArrayType *arrayType) {
  assert(arrayType && "ArrayType cannot be null");
  std::vector<size_t> sizes;
  while (arrayType) {
    sizes.push_back(arrayType->getSize());
    ir::Type *elemType = arrayType->getElementType();
    arrayType = dynamic_cast<ir::ArrayType *>(elemType);
  }
  return sizes;
}

size_t getTotalArraySize(const ir::ArrayType *arrayType) {
  assert(arrayType && "ArrayType cannot be null");
  std::vector<size_t> dims = getArrayDimensions(arrayType);
  return std::accumulate(dims.begin(), dims.end(), 1ULL,
                         std::multiplies<size_t>());
}
