//===-- Unittest/UnittestTree.cpp ---------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests for the generic tree implementation.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Exception.h"
#include "serialbox/Core/TreeBase.h"
#include "serialbox/Core/Type.h"
#include <boost/any.hpp>
#include <gtest/gtest.h>

using namespace serialbox;

namespace {

class Node : public TreeNodeBase<Node> {
public:
  using Base = TreeNodeBase<Node>;

  template <class T>
  Node(std::string identifier, const T& value)
      : Base(identifier), value_(boost::any(value)), id_(ToTypeID<T>::value) {}

  Node(Node&&) = default;
  Node& operator=(Node&&) = default;
  Node(const Node&) = delete;
  Node& operator=(const Node) = delete;

  boost::any value() { return value_; }

  TypeID id() { return id_; }

  std::string toStringImpl() const {
    auto str = std::string(identifier()) + " <" + TypeUtil::toString(id_) + " : ";
    
    if(const bool* ptr = boost::any_cast<bool>(&value_))
      str += std::to_string(*ptr);
    else if(const float* ptr = boost::any_cast<float>(&value_))
      str += std::to_string(*ptr);
    else if(const double* ptr = boost::any_cast<double>(&value_))
      str += std::to_string(*ptr);
    else if(const int* ptr = boost::any_cast<int>(&value_))
      str += std::to_string(*ptr);
    else if(const std::int64_t* ptr = boost::any_cast<std::int64_t>(&value_))
      str += std::to_string(*ptr);
    else if(const std::string* ptr = boost::any_cast<std::string>(&value_))
      str += "\"" + *ptr + "\"";
    else
      throw Exception("bad any cast of type: '%s", TypeUtil::toString(id_));

    str += ">";
    return str;
  }

private:
  boost::any value_;
  TypeID id_;
};

using DummyTree = TreeBase<Node>;

class TreeTest : public testing::Test {
public:
  std::shared_ptr<DummyTree> treePtr;

protected:
  virtual void SetUp() override {
    //
    // Test tree
    //
    // Hierarchy           | Value        | Type
    // ------------------------------------------------------
    //  A                  | 55           | int
    //     A_1             | 6.2          | double
    //        A_1_1        | "string"     | std::string
    //        A_1_2        | true         | bool
    //     A_2             | 2            | int
    //        A_2_1        | 1.62         | double
    //           A_2_1_1   | "string2"    | std::string
    //  B                  | false        | bool
    //     B_1             | 42           | int
    //

    auto root = std::make_shared<Node>("__root__", std::string("__root__"));
    treePtr = std::make_shared<DummyTree>(root);

    auto A = std::make_shared<Node>("A", int(55));
    auto A_1 = std::make_shared<Node>("A_1", double(6.2));
    auto A_1_1 = std::make_shared<Node>("A_1_1", std::string("string"));
    auto A_1_2 = std::make_shared<Node>("A_1_2", bool(true));
    auto A_2 = std::make_shared<Node>("A_2", int(2));
    auto A_2_1 = std::make_shared<Node>("A_2_1", double(1.62));
    auto A_2_1_1 = std::make_shared<Node>("A_2_1_1", std::string("string2"));

    auto B = std::make_shared<Node>("B", bool(false));
    auto B_1 = std::make_shared<Node>("B_1", int(42));

    ASSERT_TRUE(treePtr->root()->addChild(A));

    ASSERT_TRUE(A->addChild(A_1));
    ASSERT_TRUE(A_1->addChild(A_1_1));
    ASSERT_TRUE(A_1->addChild(A_1_2));

    ASSERT_TRUE(A->addChild(A_2));
    ASSERT_TRUE(A_2->addChild(A_2_1));
    ASSERT_TRUE(A_2_1->addChild(A_2_1_1));

    ASSERT_TRUE(treePtr->root()->addChild(B));
    ASSERT_TRUE(B->addChild(B_1));
  }

  virtual void TearDown() override {}
};
}

TEST_F(TreeTest, Construction) {
  TreeBase<Node>& tree = *treePtr;

  EXPECT_FALSE(tree.empty());
  
  // Check nodes by descending depth-first-search
  EXPECT_TRUE(tree.find("A", DummyTree::IterationOrderKind::DFS));
  EXPECT_TRUE(tree.find("A_1", DummyTree::IterationOrderKind::DFS));
  EXPECT_TRUE(tree.find("A_1_1", DummyTree::IterationOrderKind::DFS));
  EXPECT_TRUE(tree.find("A_1_2", DummyTree::IterationOrderKind::DFS));
  EXPECT_TRUE(tree.find("A_2", DummyTree::IterationOrderKind::DFS));
  EXPECT_TRUE(tree.find("A_2_1", DummyTree::IterationOrderKind::DFS));
  EXPECT_TRUE(tree.find("A_2_1_1", DummyTree::IterationOrderKind::DFS));
  EXPECT_TRUE(tree.find("B", DummyTree::IterationOrderKind::DFS));
  EXPECT_TRUE(tree.find("B_1", DummyTree::IterationOrderKind::DFS));
  EXPECT_FALSE(tree.find("C", DummyTree::IterationOrderKind::DFS));
  
  // Check nodes by descending breadth-first-search  
  EXPECT_TRUE(tree.find("A", DummyTree::IterationOrderKind::BFS));
  EXPECT_TRUE(tree.find("A_1", DummyTree::IterationOrderKind::BFS));
  EXPECT_TRUE(tree.find("A_1_1", DummyTree::IterationOrderKind::BFS));
  EXPECT_TRUE(tree.find("A_1_2", DummyTree::IterationOrderKind::BFS));
  EXPECT_TRUE(tree.find("A_2", DummyTree::IterationOrderKind::BFS));
  EXPECT_TRUE(tree.find("A_2_1", DummyTree::IterationOrderKind::BFS));
  EXPECT_TRUE(tree.find("A_2_1_1", DummyTree::IterationOrderKind::BFS));
  EXPECT_TRUE(tree.find("B", DummyTree::IterationOrderKind::BFS));
  EXPECT_TRUE(tree.find("B_1", DummyTree::IterationOrderKind::BFS));
  EXPECT_FALSE(tree.find("C", DummyTree::IterationOrderKind::BFS));
  
  // Check values
  EXPECT_EQ(boost::any_cast<int>(tree.find("A")->value()), 55);
  EXPECT_EQ(boost::any_cast<double>(tree.find("A_1")->value()), 6.2);
  EXPECT_EQ(boost::any_cast<std::string>(tree.find("A_1_1")->value()), std::string("string"));
  EXPECT_EQ(boost::any_cast<bool>(tree.find("A_1_2")->value()), true);
  EXPECT_EQ(boost::any_cast<int>(tree.find("A_2")->value()), 2);
  EXPECT_EQ(boost::any_cast<double>(tree.find("A_2_1")->value()), 1.62);
  EXPECT_EQ(boost::any_cast<std::string>(tree.find("A_2_1_1")->value()), std::string("string2"));
  EXPECT_EQ(boost::any_cast<bool>(tree.find("B")->value()), false);
  EXPECT_EQ(boost::any_cast<int>(tree.find("B_1")->value()), 42);
}

TEST_F(TreeTest, ModifyNodes) {
  Logging::SeverityKind severity = Logging::getMinLogLevel();
  Logging::setMinLogLevel(Logging::SeverityKind::Error);  
  
  TreeBase<Node>& tree = *treePtr;
  
  // Add newNode1 at A_2_1_1
  auto node = tree.find("A_2_1_1");
  ASSERT_NE(node, nullptr);
  
  {
    auto newNode1 = std::make_shared<Node>("newNode1", std::string("val"));
    node->addChild(newNode1);

    EXPECT_TRUE(node->childExists("newNode1"));
    EXPECT_TRUE(tree.exists("newNode1"));
    EXPECT_EQ(newNode1.use_count(), 2);
  }
  
  // Release local refrence and check if node still exists
  EXPECT_TRUE(node->childExists("newNode1"));
  EXPECT_TRUE(tree.exists("newNode1"));
  EXPECT_STREQ(boost::any_cast<std::string>(tree.find("newNode1")->value()).c_str(), "val");
  
  {
    // Remove newNode
    auto newNode = tree.find("newNode1");
    ASSERT_TRUE(tree.remove("newNode1"));
    EXPECT_FALSE(tree.remove("newNode1"));
    EXPECT_TRUE(newNode.unique());
  }
  
  // Remove B (check if all children are removed)
  auto B = tree.find("B");
  auto B_1 = tree.find("B_1");
  
  ASSERT_NE(B, nullptr);
  ASSERT_NE(B_1, nullptr);

  ASSERT_TRUE(tree.remove("B"));
  EXPECT_FALSE(tree.exists("B"));
  EXPECT_EQ(B.use_count(), 1);  
  EXPECT_EQ(B_1.use_count(), 2);
  B.reset();
  EXPECT_TRUE(B_1.unique());
  
  // Add newNode2 at root
  auto newNode2 = std::make_shared<Node>("newNode2", std::string("newNode2"));  
  EXPECT_TRUE(tree.insertAtRoot(newNode2));
  EXPECT_FALSE(tree.insertAtRoot(newNode2));
  EXPECT_TRUE(tree.exists("newNode2"));
  
  Logging::setMinLogLevel(severity);    
}
