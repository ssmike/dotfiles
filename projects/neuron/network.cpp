#include "network.h"
#include <random>
#include <cmath>
#include <limits>

namespace {
    std::default_random_engine re;
    double activation_function(double x) {
        return 2/(1+exp(-x))-1;
    }
    double activation_function_derivative(double x) {
        return 2*exp(x)/(exp(x)+1)/(exp(x)+1);
    }
}

Link::Link(Producer & source, Consumer & target): source(source), target(target) {
    double init_lower_bound = -0.5;
    double init_upper_bound = 0.5;
    std::uniform_real_distribution<double> unif(init_lower_bound, init_upper_bound);
    weight = unif(::re);
    source.addReceiver(*this);
    target.addSource(*this);
}

void Neuron::resetOutput() {
    energy = std::numeric_limits<double>::quiet_NaN();
}

void Neuron::addReceiver(Link & _l) {
    outputs.push_back(&_l);
}

void Neuron::addSource(Link & _l) {
    inputs.push_back(&_l);
}

Neuron::Neuron() {
    resetOutput();
    resetError();
}

void Neuron::resetError() {
    error = std::numeric_limits<double>::quiet_NaN();
}

double NetworkOutput::getState() {
    return getSignal();
}

void NetworkOutput::teach(double x) {
    error = (x - getSignal()) * activation_function_derivative(getSignal());
}

void Neuron::calcEnergy() {
    if (energy == energy) return;
    double sum = 0;
    for (Link * i: inputs) {
        sum += i->getSignal();
    }
    energy = ::activation_function(sum + shift);
}

double Link::getError() {
    return weight * target.getError();
}

void Neuron::calcError() {
    if (error == error) return;
    calcEnergy();
    double sum = 0;
    for (Link * i: outputs) {
        sum += i->getError();
    }
    error = ::activation_function_derivative(energy) * sum;
}

double Neuron::getError() {
    calcError();
    return error;
}

double Neuron::getSignal() {
    calcEnergy();
    return energy;
}

void NetworkInput::setState(double x) {
    signal = x;
}

double Link::getSignal() {
    return source.getSignal() * weight;
}

double NetworkInput::getSignal() {
    return signal;
}

void NetworkInput::addReceiver(Link&){}

void Link::changeWeight() {
    weight -= learning_rate * target.getError() * source.getSignal();
}

void Neuron::changeShift() {
    shift -= learning_rate * error;
}
