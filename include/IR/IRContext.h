#ifndef __IR_IRCONTEXT_H__
#define __IR_IRCONTEXT_H__

#include <memory>
#include <vector>

#include "IR/Object.h"

namespace ir {

class IRContext {
public:
  template <typename T, typename... Args> T *create(Args &&...args) {
    auto ptr = std::make_unique<T>(*this, std::forward<Args>(args)...);
    T *raw_ptr = ptr.get();
    objects_.push_back(std::move(ptr));
    return raw_ptr;
  }

  ~IRContext() = default;

private:
  std::vector<std::unique_ptr<IRObject>> objects_;
};

} // namespace ir

#endif // __IR_IRCONTEXT_H__
