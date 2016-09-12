//===-- serialbox/Core/TreeBase.h ---------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Generic implementation of a n-ary tree.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_TREE_H
#define SERIALBOX_CORE_TREE_H

#include "serialbox/Core/Logging.h"
#include <boost/noncopyable.hpp>
#include <boost/utility/string_ref.hpp>
#include <iosfwd>
#include <memory>
#include <vector>

namespace serialbox {

//===------------------------------------------------------------------------------------------===//
//    Node
//===------------------------------------------------------------------------------------------===//

/// \brief CRTP base class of tree node
template <class DerivedNodeType>
class TreeNodeBase : public std::enable_shared_from_this<DerivedNodeType> {
public:
  /// \name Constructors
  /// @{

  /// \brief Initialize node
  ///
  /// \param identifier   Identifier of the node
  TreeNodeBase(boost::string_ref identifier) : identifier_(identifier) {}

  /// \brief Move constructor
  TreeNodeBase(TreeNodeBase&&) = default;

  /// \brief Move assignment
  TreeNodeBase& operator=(TreeNodeBase&&) = default;

  /// \brief Copy constructor (\b deleted)
  TreeNodeBase(const TreeNodeBase&) = delete;

  /// \brief Copy assignment (\b deleted)
  TreeNodeBase& operator=(const TreeNodeBase&) = delete;

  /// \brief Destructor
  ~TreeNodeBase() {
    for(auto& child : children_)
      child.reset();
  }
  
  /// @}
  /// \name Modify children
  /// @{

  /// \brief Add a child node
  ///
  /// \param identifier   Identifier of the node
  /// \return True if child was succesfully added, false if a child with \c identifier does already
  /// exist
  bool addChild(const std::shared_ptr<DerivedNodeType>& child) noexcept {
    if(childExists(child->identifier())) {
      LOG(WARNING) << "cannot add child '" << child->identifier() << "' to '" << identifier_
                   << ": child with given identifier already exists";
      return false;
    }
    children_.push_back(child);
    return true;
  }

  /// \brief Remove child with given identifier
  ///
  /// \param identifier   Identifier of the node to be removed
  /// \return True if child was sucessfully removed, false if no child with \c identifier exists
  bool removeChild(boost::string_ref identifier) noexcept {
    auto childIter = std::remove_if(children_.begin(), children_.end(),
                                    [&](const std::shared_ptr<DerivedNodeType>& child) {
                                      return (child->identifier() == identifier);
                                    });

    // My children are save check the children of the children
    if(children_.end() == childIter) {
      for(auto& child : children_)
        if(child->removeChild(identifier))
          return true;
      return false;
    }

    children_.erase(childIter, children_.end());
    return true;
  }
 
  /// \brief Get vector of children
  const std::vector<std::shared_ptr<DerivedNodeType>>& children() const noexcept {
    return children_;
  }
  std::vector<std::shared_ptr<DerivedNodeType>>& children() noexcept { return children_; }

  /// \brief Does the node have any children?
  bool hasChildren() const noexcept { return !children_.empty(); }

  /// \brief Search node and all child nodes for node with given identifier
  ///
  /// Implemented as depth-first-search.
  ///
  /// \param identifier   Identifier of the child node
  /// \return Pointer to child node or nullptr if no child with \c identifier exists
  std::shared_ptr<DerivedNodeType> findDFS(boost::string_ref identifier) noexcept {

    // Are you looking for me?
    if(identifier == identifier_)
      return sharedFromThis();

    // Recursivly check children
    for(auto& child : children_) {
      const auto& node = child->findDFS(identifier);
      if(node != nullptr)
        return node;
    }
    return std::shared_ptr<DerivedNodeType>();
  }

  /// \brief Search node and all child nodes for node with given identifier
  ///
  /// Implemented as breadth-first-search.
  ///
  /// \param identifier   Identifier of the child node
  /// \return Pointer to child node or nullptr if no child with \c identifier exists
  std::shared_ptr<DerivedNodeType> findBFS(boost::string_ref identifier) noexcept {

    // Check children
    for(const auto& child : children_) {
      if(child->identifier() == identifier)
        return child;
    }

    // Recursivly descend
    for(auto& child : children_) {
      const auto& node = child->findBFS(identifier);
      if(node != nullptr)
        return node;
    }
    return std::shared_ptr<DerivedNodeType>();
  }

  /// \brief Check if child with given identifier exists
  ///
  /// \param identifier   Identifier the of child node
  /// \return True iff a child with \c identifier exists
  bool childExists(boost::string_ref identifier) noexcept {
    for(const auto& child : children_) {
      if(child->identifier() == identifier)
        return true;
    }
    return false;
  }

  /// \brief Number of children
  std::size_t numChildren() const noexcept { return children_.size(); }

  /// @}

  /// \brief Get a \c shared_ptr to \c this
  ///
  /// It is permitted to call sharedFromThis only on a previously shared object, i.e. on an object
  /// managed by \c std::shared_ptr<SavepointTreeNode>. Otherwise the behavior is undefined.
  std::shared_ptr<DerivedNodeType> sharedFromThis() {
    return static_cast<DerivedNodeType&>(*this).shared_from_this();
  }

  /// \brief Get identifier
  boost::string_ref identifier() const noexcept { return identifier_; }

  /// \brief Convert node (and all its children) to a multi-line string
  std::string toString(std::size_t indent) const noexcept {
    std::string nodeStr(std::string(indent, ' ') +
                        static_cast<const DerivedNodeType&>(*this).toStringImpl() + "\n");
    for(const auto& child : children_)
      nodeStr += child->toString(indent + 4);
    return nodeStr;
  }

protected:
  std::string identifier_;                                 ///< Node identifier
  std::vector<std::shared_ptr<DerivedNodeType>> children_; ///< Pointer to children
};

//===------------------------------------------------------------------------------------------===//
//    Tree
//===------------------------------------------------------------------------------------------===//

/// \brief Generic implementation of a n-ary tree.
template <class NodeType>
class TreeBase : private boost::noncopyable {
public:
  /// \brief Order in which nodes are proccessed in an iteration
  enum IterationOrderKind {
    DFS, ///< Breadth-first-search
    BFS  ///< Depth-first-search
  };

  /// \brief Construct tree rooted at \c root
  explicit TreeBase(const std::shared_ptr<NodeType>& root) {
    CHECK(root != nullptr);
    root_ = root;
  }

  /// \brief Add a child node to the node given by identifier
  ///
  /// \param identifier   Identifier of the node
  /// \return True if child was succesfully added, false if a child with \c identifier does already
  /// exist
  bool insert(boost::string_ref identifier, const std::shared_ptr<NodeType>& child) noexcept {
    std::shared_ptr<NodeType> node = this->find(identifier);
    return node ? node->addChild(child) : false;
  }
  
  /// \brief Add node given by identifier as a child of root
  ///
  /// \param identifier   Identifier of the node
  /// \return True if child was succesfully added, false if a child with \c identifier does already
  /// exists
  bool insertAtRoot(const std::shared_ptr<NodeType>& child) noexcept {
    return root_->addChild(child);;
  }

  /// \brief Get node with given identifier (Depth-first-search)
  /// \return Pointer to the node or nullptr if no node with \c identifier exists
  std::shared_ptr<NodeType>
  find(boost::string_ref identifier,
       IterationOrderKind iterationOrder = IterationOrderKind::DFS) noexcept {
    if(iterationOrder == IterationOrderKind::DFS)
      return root_->findDFS(identifier);
    else
      return root_->findBFS(identifier);
  }

  /// \brief Check if node with given identifier exists
  bool exists(boost::string_ref identifier) noexcept { return (this->find(identifier) != nullptr); }

  /// \brief Remove child with given identifier
  ///
  /// \param identifier   Identifier of the node to be removed
  /// \return True if node was sucessfully removed, false if no node with \c identifier exists
  bool remove(boost::string_ref identifier) noexcept {
    return (root_->removeChild(identifier));
  }

  /// \brief Get root node
  const std::shared_ptr<NodeType>& root() const noexcept { return root_; }
  std::shared_ptr<NodeType>& root() noexcept { return root_; }

  /// \brief Check if tree is empty
  bool empty() const noexcept { return (!root_->hasChildren()); }

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const TreeBase& s) {
    return (stream << s.root()->toString(0));
  }

private:
  std::shared_ptr<NodeType> root_;
};

} // namespace serialbox

#endif
